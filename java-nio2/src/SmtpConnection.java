import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousSocketChannel;
import java.util.Locale;
import java.util.List;
import java.util.ArrayList;
import java.io.UnsupportedEncodingException;

public class SmtpConnection implements AsynchronousConnectionHandler, SmtpSession {
    
    private AsynchronousConnection connection;
    //private String clientIp;
    private static final int WRITE_TIMEOUT = 60;
    private static final int READ_TIMEOUT = 60;
    
    // state fields
    private String hostname;
    private String heloName = null;
    private String sender = null;
    private List<String> recipients;
    private boolean dataMode = false;
    private boolean quitting = false;
    
    public SmtpConnection(AsynchronousSocketChannel socketChannel, String hostname) {
        connection = new AsynchronousConnection(socketChannel, this);
        this.hostname = hostname;
        recipients = new ArrayList<String>();
        // start the conversation
        SmtpReply reply = new SmtpReply(220, hostname + " ESMTP"); // Wed, 29 Jun 2011 10:54:17 +0000
        // "554 5.7.1 <[#{remote_addr}]>: Client host rejected: Access denied"
        // "421 #{@hostname} Service unavailable - try again later"
        connection.write(reply.toString(), WRITE_TIMEOUT);
    }
    
    /*
    public String getRemoteIP() throws IOException {
        InetSocketAddress socketAddress = (InetSocketAddress) socketChannel.getRemoteAddress();
        InetAddress address = socketAddress.getAddress();
        address.getHostName(); // causes toString() to print the name too
        String ip = address.toString();
        // "/127.0.0.1"
        return ip;
    }
    */

    //public void setClientIp(clientIp) {
    //    this.clientIp = clientIp;
    //}
    
    //
    // callbacks
    //

    public void writeCompleted(int nWritten) {
        if (quitting == false) {
            // read with timeout and completion handler
            connection.read(READ_TIMEOUT);
        }
        else {
            connection.close();
        }
    }

    public void writeFailed(Throwable e) {
        System.err.println("Write failed with " + e.toString());
        connection.close();
    }

    public void readCompleted(ByteBuffer buffer) {
        List<String> replies = new ArrayList<String>();
        for (String line: ByteBuffers.getLines(buffer)) {
            SmtpReply reply = processLine(line);
            if (reply != null) {
                replies.add(reply.toString());
            }
        }

        // trigger new action to keep conversation going
        if (replies.size() > 0) {
            connection.write(replies.toArray(new String[replies.size()]), WRITE_TIMEOUT);
        }
        else {
            assert dataMode == true;
            connection.read(READ_TIMEOUT);
        }
    }
    
    public void readFailed(Throwable e) {
        System.err.println("Read failed with " + e.toString());
        // ? reply with error
        connection.close();
    }

    public void disconnected() {
        System.out.println("Peer disconnected");
    }
    
    //
    // private helpers
    //

    private SmtpReply processLine(String line) {
        //System.err.println(">> " + line);
        SmtpReply reply = null;
        if (dataMode == false) {
            reply = SmtpCommand.handler(this, line);
        }
        else {
            // check for CRLF.CRLF or LF.LF
            assert line.length() > 0;  // at least \n
            if (line.charAt(0) == '.') {
                if (line.charAt(1) == '\r' || line.charAt(1) == '\n') {
                    // ? do not allow .CR as end-of-message?
                    reply = endOfData();
                }
                else {
                    // unstuff extra dot
                }
            }
        }
        return reply;
    }
    
    private SmtpReply endOfData() {
        //System.out.println("End data transfer mode");
        dataMode = false;
        return new SmtpReply(250, "2.0.0 Ok");
    }
    
    /*
     * SmtpSession interface
     */

    public String getHostName() {
        return hostname;
    }
    
    public void setHeloName(String heloName){
        this.heloName = heloName;
    }
    
    public String getHeloName() {
        return heloName;
    }

    public void setSender(String sender) {
        this.sender = sender;
    }

    public String getSender() {
        return sender;
    }

    public void addRecipient(String recipient) {
        recipients.add(recipient);
    }

    public List<String> getRecipients() {
        return recipients;
    }

    public void startData() {
        dataMode = true;
        //System.out.println("Start data transfer mode");
    }

    public void reset() {
        sender = null;
        recipients.clear();
    }

    public void disconnect() {
        quitting = true;
    }
}
