/*
 * SMTP Daemon in C with libevent
 *
 * Compile with:
 * cc -I/usr/local/include -o smtpd smtpd.c -L/usr/local/lib -levent
 *
 * Copyright (c) 2007-2012 Maarten Oelering
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

/* Required by event.h. */
#include <sys/time.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <err.h>

/* Libevent. */
#include <event.h>

/* Port to listen on. */
#define SERVER_PORT 2525

/* states */
#define WAIT_EHLO 1
#define WAIT_MAIL 2
#define WAIT_RCPT 3
#define WAIT_DATA 4
#define WAIT_QUIT 5

/**
 * A struct for client specific data, also includes pointer to create
 * a list of clients.
 */
struct client {
	/* The clients socket. */
	int fd;

	/* The bufferedevent for this client. */
	struct bufferevent *buf_ev;
	
	int state;
	
	/* info received from client */
	char* ip;
	char* hostname;
	char* mail_from;
	char** rcpt_to;
	char* data;
};

/*
 * Compile with:
 * cc -I/usr/local/include -o event-test event-test.c -L/usr/local/lib -levent
 */

/**
 * Set a socket to non-blocking mode.
 */
int
setnonblock(int fd)
{
	int flags;

	/* ??? WIN32: use ioctlsocket() */
	flags = fcntl(fd, F_GETFL);
	if (flags < 0)
		return flags;
	flags |= O_NONBLOCK;
	if (fcntl(fd, F_SETFL, flags) < 0)
		return -1;

	return 0;
}

void write_banner(struct client* client)
{
	char message[1024]; /* static ? */
	snprintf(message, sizeof(message), "220 %s ESMTP %s\r\n", "hostname", "version");
	
	bufferevent_write(client->buf_ev, message, strlen(message));
}

void write_ehlo_rsp(struct client* client)
{
	char message[1024]; /* static ? */
	snprintf(message, sizeof(message), "250-%s\r\n250-SIZE %s\r\n250 8BITMIME", 
		"hostname", "maxsize");
	
	bufferevent_write(client->buf_ev, message, strlen(message));
}

/**
 * Called by libevent when there is data to read.
 */
void
buffered_on_read(struct bufferevent *bev, void *arg)
{
	struct client *client = (struct client *)arg;
	char* line;
	
	printf("buffered_on_read\n");
	line = evbuffer_readline(bev->input);
	printf("%s\n", line);

	if (strncasecmp(line, "HELO", 4) == 0)
	{
		/* we expect EHLO */
	}
	else if (strncasecmp(line, "EHLO", 4) == 0)
	{
		if (client->state == WAIT_EHLO)
		{
			write_ehlo_rsp(client);
			client->state = WAIT_MAIL;
		}
	}
	else if (strncasecmp(line, "MAIL", 4) == 0)
	{
		/* we don't allow skipping EHLO */
		/* "503 5.5.1 Error: send HELO/EHLO first" */
		/* "501 5.5.4 Syntax: MAIL FROM:<address>" */
		/* "501 5.1.7 Bad sender address syntax" */
		/* expect BODY=8BITMIME, BODY=7BIT, SIZE=, RET=, ENVID= */ 
		/* 250 2.1.0 Ok */
	}
	else if (strncasecmp(line, "RCPT", 4) == 0)
	{
		/* "250 Ok" */
		/* "501 Error: invalid address" */
	}
	else if (strncasecmp(line, "DATA", 4) == 0)
	{
		/* "354 End data with <CR><LF>.<CR><LF>" */
		/* 250 Ok: queued as 7BABF7B086 */
	}
	else if (strncasecmp(line, "RSET", 4) == 0)
	{
		/*
	     * Restore state to right after HELO/EHLO command.
	     */
		/* "250 2.0.0 Ok" */
		/* no parameters allowed "501 5.5.4 Syntax: RSET" */
	}
	else if (strncasecmp(line, "NOOP", 4) == 0)
	{
		/* RFC 2821 says that NOOP can have a parameter string which is to be ignored */
		/* "250 2.0.0 Ok" */		
	}
	else if (strncasecmp(line, "QUIT", 4) == 0)
	{
		/* "221 2.0.0 Bye" */
	}
	else
	{
		/* "502 Error: command not implemented" */
	}
}

/**
 * Called by libevent when the write buffer reaches 0.  We only
 * provide this because libevent expects it, but we don't use it.
 */
void
buffered_on_write(struct bufferevent *bev, void *arg)
{
	printf("buffered_on_write\n");
}

/**
 * Called by libevent when there is an error on the underlying socket
 * descriptor.
 */
void
buffered_on_error(struct bufferevent *bev, short what, void *arg)
{
	struct client *client = (struct client *)arg;

	if (what & EVBUFFER_EOF) {
		/* Client disconnected, remove the read event and the
		 * free the client structure. */
		printf("Client disconnected.\n");
	}
	else {
		warn("Client socket error, disconnecting.\n");
	}
	bufferevent_free(client->buf_ev);
	/* close client socket */
	close(client->fd);
	free(client);
}

/*
*/
void
on_accept(int fd, short ev, void *arg)
{
	int client_fd;
	struct sockaddr_in client_addr;
	socklen_t client_len = sizeof(client_addr);
	struct client *client;
	char* client_ip;

	/* return next completed connection */
	client_fd = accept(fd, (struct sockaddr *)&client_addr, &client_len);
	if (client_fd < 0) {
		warn("accept failed");
		return;
	}

	client_ip = inet_ntoa(client_addr.sin_addr);
	printf("Accepted connection from %s\n", client_ip);

	/* Set the client socket to non-blocking mode. */
	if (setnonblock(client_fd) < 0)
		warn("failed to set client socket non-blocking");

	/* We've accepted a new client, create a client object. */
	client = calloc(1, sizeof(*client));
	if (client == NULL)
		err(1, "malloc failed");
	client->fd = client_fd;
	
	/* save client ip address */
	client->ip = client_ip;
	
	client->state = WAIT_EHLO;

/*
 * Create a new buffered event object.
 *
 * The read callback is invoked whenever we read new data.
 * The write callback is invoked whenever the output buffer is drained.
 * The error callback is invoked on a write/read error or on EOF.
 * Also sets EV_WRITE.
 */
	client->buf_ev = bufferevent_new(client_fd, buffered_on_read,
	    buffered_on_write, buffered_on_error, client);

	/* We have to enable it before our callbacks will be
	 * called. */
	bufferevent_enable(client->buf_ev, EV_READ);

	write_banner(client);
}

static int 
make_socket(int port)
{
	int listen_fd;
	struct sockaddr_in listen_addr;
	int reuseaddr_on;

	/* create socket of family IPv4 and type TCP */
	listen_fd = socket(AF_INET, SOCK_STREAM, 0);
	if (listen_fd < 0)
		err(1, "listen failed");

	/* allow local address reuse */
	/* ??? should be called between socket and bind */
	reuseaddr_on = 1;
	setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &reuseaddr_on, sizeof(reuseaddr_on));
	/* ??? SO_LINGER */

	/* assign local protocol address to socket */
	memset(&listen_addr, 0, sizeof(listen_addr));
	listen_addr.sin_family = AF_INET;
	listen_addr.sin_addr.s_addr = htonl(INADDR_ANY);  /* bind to any IP */
	listen_addr.sin_port = htons(port);
	if (bind(listen_fd, (struct sockaddr *) &listen_addr, sizeof(listen_addr)) < 0)
		err(1, "bind failed");
	
	/* convert socket to listening with backlog size of 5 */	
	if (listen(listen_fd, 5) < 0)
		err(1, "listen failed");

	/* Set the socket to non-blocking, this is essential in event
	 * based programming with libevent. */
	
	/* ??? do after socket() */
	if (setnonblock(listen_fd) < 0)
		err(1, "failed to set server socket to non-blocking");

	return listen_fd;
}

int 
main(int argc, char **argv)
{
	int listen_fd;
	struct event ev_accept;

	/* Initialize libevent. */
	event_init();
	
	listen_fd = make_socket(SERVER_PORT);

	/* We now have a listening socket, we create a read event to
	 * be notified when a client connects. */
	event_set(&ev_accept, listen_fd, EV_READ|EV_PERSIST, on_accept, NULL);
	event_add(&ev_accept, NULL);

	/* Start the event loop. */
	event_dispatch();

	return 0;
}
