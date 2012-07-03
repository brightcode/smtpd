import java.nio.ByteBuffer;

/**
 * Callback functions from AsynchronousConnection.
 *
 * @author Maarten Oelering
 */
public interface AsynchronousConnectionHandler {
    
    public void writeCompleted(int nWritten);

    public void writeFailed(Throwable e);
    
    public void readCompleted(ByteBuffer buffer);
    
    public void readFailed(Throwable e);
    
    public void disconnected();
}

