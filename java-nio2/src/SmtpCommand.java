
/**
 * Typesafe enum class
 * package private
 */
enum SmtpCommand {
    HELO() {
        public SmtpReply execute(SmtpSession session, String args) {
            // accept empty or non fqdn helo names (like most mail servers)
            session.setHeloName(args);
            // basic response without chit chat
            return new SmtpReply(250, session.getHostName());
        }
    },
    EHLO() {
        public SmtpReply execute(SmtpSession session, String args) {
            // accept empty or non fqdn helo names
            session.setHeloName(args);
            // respond with supported extensions
            return new SmtpReply(250, session.getHostName(), "PIPELINING", "8BITMIME", "ENHANCEDSTATUSCODES");
        }
    },
    VRFY() {
        public SmtpReply execute(SmtpSession session, String args) {
            return new SmtpReply(502, "5.5.1 VRFY command is disabled");
        }
    },
    MAIL() {
        public SmtpReply execute(SmtpSession session, String args) {
            // accept mail without helo (like postfix, exim, momentum, powermta)
            if (args.regionMatches(true, 0, "FROM:", 0, 5) == false) {
                return new SmtpReply(500, "5.5.2 Syntax: MAIL FROM:<address>");
            }
            // reject empty address without brackets
            String sender = args.substring(5).trim();
            if (sender.length() == 0) {
                return new SmtpReply(501, "5.5.2 Syntax: MAIL FROM:<address>");
            }
            // todo: test address syntax
            // accept valid address without brackets
            session.setSender(sender);
            return new SmtpReply(250, "2.1.0 Ok");
        }
    },
    RCPT() {
        public SmtpReply execute(SmtpSession session, String args) {
            if (session.getSender() == null) {
                return new SmtpReply(503, "5.5.1 Specify sender first");
            }
            if (args.regionMatches(true, 0, "TO:", 0, 3) == false) {
                return new SmtpReply(500, "5.5.2 Syntax: RCPT TO:<address>");
            }
            String recipient = args.substring(3).trim();
            if (recipient.length() == 0) {
                return new SmtpReply(501, "5.5.2 Syntax: RCPT TO:<address>");
            }
            // todo: test address syntax
            session.addRecipient(recipient);
            return new SmtpReply(250, "2.1.0 Ok");
        }
    },
    DATA() {
        public SmtpReply execute(SmtpSession session, String args) {
            if (session.getRecipients() == null || session.getRecipients().size() == 0) {
                return new SmtpReply(503, "5.5.1 No valid recipients");
            }
            // start data transfer mode
            session.startData();
            return new SmtpReply(354, "End data with <CR><LF>.<CR><LF>");
        }
    },
    RSET() {
        public SmtpReply execute(SmtpSession session, String args) {
            // reset transaction
            session.reset();
            return new SmtpReply(250, "2.0.0 Ok");
        }
    },
    QUIT() {
        public SmtpReply execute(SmtpSession session, String args) {
            // close connection after write completed
            session.disconnect();
            return new SmtpReply(221, "2.0.0 " + session.getHostName() + " closing connection");
        }
    };
    
    public abstract SmtpReply execute(SmtpSession session, String args);

    /**
     * Parse command and dispatch to it's handler
     */
    public static SmtpReply handler(SmtpSession session, String command) {
        if (command.length() < 4) {
            return new SmtpReply(500, "5.5.2 Error: bad syntax");
        }
        
        String name = command.substring(0, 4).toUpperCase(); // (Locale.ENGLISH);
        SmtpCommand smtpCommand;
        try {
            // find SmtpCommand instance that matches the name
            smtpCommand = valueOf(name);
        } 
        catch (IllegalArgumentException e) {
            return new SmtpReply(500, "5.5.2 Command not recognized");
        }

        String args = command.substring(4).trim();
        return smtpCommand.execute(session, args);
    }
}
