CC=gcc
CFLAGS=
LIBS=-levent
SRCS=smtpd.c
TARG=smtpd

.PHONY: all
all: $(TARG)

$(TARG):
	$(CC) $(CFLAGS) $(LIBS) $(SRCS) -o $(TARG)

clean:
	rm -f *.o $(TARG)

