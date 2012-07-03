import java.util.List;

/**
 * API for command processor
 */
public interface SmtpSession {
    
    //public void setHostName(String hostName);
    public String getHostName();

    public void setHeloName(String heloName);
    public String getHeloName();

    public void setSender(String sender);
    public String getSender();

    public void addRecipient(String recipient);
    public List<String> getRecipients();
    
    /**
     * Start data transfer mode
     */
    public void startData();

    /**
     * Abort transaction and restore state to right after HELO/EHLO
     */
    public void reset();

    /**
     * Disconnect after write completed
     */
    public void disconnect();
}
