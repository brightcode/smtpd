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
import java.io.IOException;
import java.io.UnsupportedEncodingException;

/**
 *
 */
public class AsynchronousConnection {
    
    private static class WriteCompletionHandler 
        implements CompletionHandler<Integer, AsynchronousConnection> {
            
        public void completed(Integer nWritten, AsynchronousConnection conn) {
            conn.handler.writeCompleted(nWritten.intValue());
        }

        public void failed(Throwable exc, AsynchronousConnection conn) {
            if (exc instanceof AsynchronousCloseException) {
                conn.handler.disconnected();
            }
            else {
                conn.handler.writeFailed(exc);
            }
        }
    }
    
    private static class WriteListCompletionHandler 
        implements CompletionHandler<Long, AsynchronousConnection> {
            
        public void completed(Long nWritten, AsynchronousConnection conn) {
            conn.handler.writeCompleted(nWritten.intValue());
        }

        public void failed(Throwable exc, AsynchronousConnection conn) {
            if (exc instanceof AsynchronousCloseException) {
                conn.handler.disconnected();
            }
            else {
                conn.handler.writeFailed(exc);
            }
        }
    }
    
    private static class ReadCompletionHandler
        implements CompletionHandler<Integer, AsynchronousConnection> {
        
        public void completed(Integer nRead, AsynchronousConnection conn) {
            conn.readCompleted(nRead.intValue());
        }

        public void failed(Throwable exc, AsynchronousConnection conn) {
            if (exc instanceof AsynchronousCloseException) {
                conn.handler.disconnected();
            }
            else {
                conn.handler.readFailed(exc);
            }
        }
    }

    private static CompletionHandler<Integer, AsynchronousConnection> writeHandler = new WriteCompletionHandler();
    private static CompletionHandler<Long, AsynchronousConnection> writeListHandler = new WriteListCompletionHandler();
    private static CompletionHandler<Integer, AsynchronousConnection> readHandler = new ReadCompletionHandler();
    
    private AsynchronousSocketChannel channel;
    private AsynchronousConnectionHandler handler;
    private ByteBuffer readBuffer;
    private long startTime;
    
    public AsynchronousConnection(AsynchronousSocketChannel socketChannel, AsynchronousConnectionHandler socketHandler) {
        channel = socketChannel;
        handler = socketHandler;
        readBuffer = ByteBuffer.allocate(1024);  // smtp allows 1000 characters per line (incl CRLF)
    }
    
    public void write(String s, int timeout) {
        ByteBuffer buffer = ByteBuffers.fromString(s);
        channel.write(buffer, timeout, TimeUnit.SECONDS, this, writeHandler);
    }

    public void write(String[] strings, int timeout) {
        ByteBuffer[] buffers = new ByteBuffer[strings.length];
        for (int i = 0; i < strings.length; i++) {
            buffers[i] = ByteBuffers.fromString(strings[i]);
        }
        channel.write(buffers, 0, buffers.length, timeout, TimeUnit.SECONDS, this, writeListHandler);
    }
    
    // asynchronous read
    // only one outstanding read allowed
    public void read(int timeout) {
        // all data read?
        if (readBuffer.remaining() == 0) {
            readBuffer.clear(); // reset position to zero and limit to capacity
        }
        // not fresh buffer?
        else if (readBuffer.remaining() != readBuffer.capacity()) {
            readBuffer.compact(); // shift unread data to beginning, set position after unread data
            //System.err.println(readBuffer.toString());
        }

        startTime = System.currentTimeMillis(); // System.nanoTime();
        channel.read(readBuffer, timeout, TimeUnit.SECONDS, this, readHandler);
    }
    
    private void readCompleted(int nRead) {
        if (nRead > 0) {
            readBuffer.flip();  // set limit to position, and position to zero
            // note that read may be called (once) from readCompleted
            handler.readCompleted(readBuffer);
        }
        else if (nRead == 0) {
            handler.readFailed(new IOException("Completed with zero read; buffer may be full"));
        }
        else {
            handler.readFailed(new IOException("Channel reached end-of-stream"));
        }
    }
    
    public void close() {
        if (channel.isOpen()) {
            try {
                channel.close();
            }
            catch (IOException e) {
            }
        }
    }
}

