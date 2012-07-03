import java.util.regex.Pattern;
import java.util.List;
import java.util.ArrayList;

/**
 * package-private
 */
class SmtpReply {
    
    private static final Pattern SPLIT_RE = Pattern.compile("\\s+");
    private static final int MAX_LEN = 72;

    private final int code;
    private final String[] text;
    
    public SmtpReply(int code, String... text) {
        this.code = code;
        this.text = text;
    }
    
    // ??? static factory method
    
    public String toString() {
        if (text.length == 1 && text[0].length() <= MAX_LEN) {
            return code + " " + text[0] + "\r\n";
        }
        else {
            // map array with text parts to array with lines of maximum length
            List<String> lines = new ArrayList<String>();
            for (String part: text) {
                if (part.length() <= MAX_LEN) {
                    lines.add(part);
                }
                else {
                    lines.addAll(makeLines(part, MAX_LEN));
                }
            }
            // assemble multiline response from one or more lines
            StringBuilder sb = new StringBuilder();
            String lastLine = lines.remove(lines.size() - 1);
            for (String line: lines) {
                sb.append(code).append("-").append(line).append("\r\n");
            }
            sb.append(code).append(" ").append(lastLine).append("\r\n");
            return sb.toString();
        }
    }
    
    /*
     * Split text in parts with maximum length
     */
    private static List<String> makeLines(String text, int maxLength) {
        List<String> lines = new ArrayList<String>();
        StringBuilder sb = new StringBuilder(maxLength);
        for (String token: SPLIT_RE.split(text)) {
            if (sb.length() == 0) {
                sb.append(token);
            }
            else if (sb.length() + token.length() < maxLength) {
                sb.append(" ").append(token);
            }
            else {
                lines.add(sb.toString());
                sb.setLength(0);
                sb.append(token);
            }
        }
        if (sb.length() != 0) {
            lines.add(sb.toString());
        }
        return lines;
    }

    public static void main(String args[]) throws Exception {
        SmtpReply reply1 = new SmtpReply(550, "550 Requested action not taken: mailbox unavailable");
        System.out.println(reply1.toString());
        SmtpReply reply2 = new SmtpReply(421, "4.7.0 [X.X.X.X] Our system has detected an unusual amount " +
            "of 4.7.0 unsolicited mail originating from your IP address. To protect our 4.7.0 users from spam, " +
            "mail sent from your IP address has been temporarily 4.7.0 blocked. Please visit " +
            "http://www.google.com/mail/help/bulk_mail.html 4.7.0 to review our Bulk Email Senders Guidelines. x19si27751115eeh.46");
        System.out.println(reply2.toString());
        SmtpReply reply3 = new SmtpReply(250, "mail.sendertools.org Hello oelering.demon.nl [83.160.157.77]",
            "SIZE 52428800", "PIPELINING", "HELP");
        System.out.println(reply3.toString());
    }
}
