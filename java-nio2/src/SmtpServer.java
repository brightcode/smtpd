// MO

import java.io.IOException;
import java.lang.InterruptedException;
import java.net.SocketAddress;
import java.net.InetSocketAddress;
import java.net.InetAddress;
import java.net.StandardSocketOptions;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousChannelGroup;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.nio.channels.AsynchronousCloseException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.Future;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

public class SmtpServer {

    private AsynchronousChannelGroup channelGroup;
    private AsynchronousServerSocketChannel serverSocketChannel;
    private String hostname;
    
    public SmtpServer(InetAddress bindAddress, int port) throws IOException {
        createChannelGroup(4);
        createServerSocketChannel(bindAddress, port);
        // set default hostname
        if (bindAddress != null) {
            hostname = bindAddress.getCanonicalHostName();
        }
        else {
            try {
                hostname = InetAddress.getLocalHost().getCanonicalHostName();
            }
            catch (UnknownHostException e) {
                hostname = "localhost";
            }
        }
    }
    
    protected void createChannelGroup(int nThreads) throws IOException {
        ThreadFactory threadFactory = Executors.defaultThreadFactory();
        channelGroup = AsynchronousChannelGroup.withFixedThreadPool(nThreads, threadFactory);
        // can also do AsynchronousChannelGroup.withThreadPool(Executors.newFixedThreadPool(nThreads));
    }
    
    protected void createServerSocketChannel(InetAddress bindAddress, int port) throws IOException {
        serverSocketChannel = AsynchronousServerSocketChannel.open(channelGroup);
        InetSocketAddress isa = (bindAddress != null) ? new InetSocketAddress(bindAddress, port) : new InetSocketAddress(port);
        serverSocketChannel.bind(isa, 0);  // default backlog ?50
        serverSocketChannel.setOption(StandardSocketOptions.SO_REUSEADDR, true);
        // option server.setOption(StandardSocketOptions.SO_RCVBUF, x);
    }
    
    protected void start() {
        serverSocketChannel.accept(null, new CompletionHandler<AsynchronousSocketChannel, Void>() {
            public void completed(AsynchronousSocketChannel socketChannel, Void attachment) {
                // ? socketChannel.setOption...

                SmtpConnection session = new SmtpConnection(socketChannel, hostname);

                // accept the next connection with this handler
                // ? if not shutdown
                serverSocketChannel.accept(null, this);
            }

            public void failed(Throwable exc, Void attachment) {
                if (exc instanceof AsynchronousCloseException) {
                    System.out.println("Accept aborted");
                }
                else {
                    System.err.println("Accept failed with " + exc.toString());
                }
            }
        });
    }
    
    public void close() throws IOException, InterruptedException {
        if (serverSocketChannel.isOpen()) {
            // close server socket channel aborting accept
            System.err.println("Closing server socket...");
            serverSocketChannel.close();
        }
        if (!channelGroup.isShutdown()) {
            // initiate orderly shutdown of other channels
            System.out.println("Orderly shutdown...");
            channelGroup.shutdown();
        }
        if (channelGroup.awaitTermination(5, TimeUnit.SECONDS) == false) {
            System.out.println("Timeout waiting for termination");
        }
        if (!channelGroup.isTerminated()) {
            // close all open channels
            System.out.println("Forceful shutdown...");
            channelGroup.shutdownNow();
        }
    }

    public static void main(String args[]) throws Exception {
        SmtpServer smtpServer = new SmtpServer(null, 2525);
        smtpServer.start();
        // Wait
        System.out.println("Press enter to stop...");
        System.in.read();

        smtpServer.close();
    }
}
