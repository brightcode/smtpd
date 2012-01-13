/**
 * Basic SMTP server in Java using Netty
 *
 * Author: Maarten Oelering
 */

import java.net.InetAddress;
import java.util.Date;

import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelEvent;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.ChannelFutureListener;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.ExceptionEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelUpstreamHandler;

public class SmtpServerHandler extends SimpleChannelUpstreamHandler 
{
    private static String hostname;
    static {
        try {
            hostname = InetAddress.getLocalHost().getHostName();
        }
        catch (java.net.UnknownHostException e) {
            hostname = "?";
        }
    }
    private boolean dataMode = false;
    private StringBuffer message = new StringBuffer();
    
    @Override
    public void channelConnected(ChannelHandlerContext ctx, ChannelStateEvent e) throws Exception 
    {
        Channel channel = e.getChannel();
        // Send greeting for a new connection.
        channel.write("220 " + hostname + " ESMTP\r\n");
    }

    @Override
    public void messageReceived(ChannelHandlerContext ctx, MessageEvent e) 
    {
        Channel channel = e.getChannel();
        // We know it is a String because we put some codec in TelnetPipelineFactory.
        String line = (String) e.getMessage();

        if (dataMode == false) {
            onCommand(channel, line);
        }
        else {
            onData(channel, line);
        }
    }
    
    private void onCommand(Channel channel, String line) 
    {
        String command;
        String parameters;
        int i = line.indexOf(" ");
        if (i > 0) {
            command = line.substring(0, i).toUpperCase();
            parameters = line.substring(i + 1);
        }
        else {
            command = line.toUpperCase();
        }
        
        if (command.length() == 0) {
            channel.write("500 Error: bad syntax\r\n");
            return;
        }   
        
        if (command.equals("HELO")) {
            channel.write("250 " + hostname + "\r\n");
        }
        else if (command.equals("EHLO")) {
            channel.write("250 " + hostname + "\r\n");			
        }
        else if (command.equals("MAIL")) {
            message.setLength(0);  // new transaction
            channel.write("250 OK\r\n");
        }
        else if (command.equals("RCPT")) {
            channel.write("250 OK\r\n");
        }
        else if (command.equals("DATA")) {
            dataMode = true;
            channel.write("354 End data with <CR><LF>.<CR><LF>\r\n");
        }
        else if (command.equals("RSET")) {
            channel.write("250 OK\r\n");
        }
        else if (command.equals("QUIT")) {
            ChannelFuture future = channel.write("221 "+ hostname + " closing connection\r\n");
            future.addListener(ChannelFutureListener.CLOSE);
        }
        else {
            channel.write("500 unrecognized command\r\n");
        }
    }
    
    private void onData(Channel channel, String line) {
        if (line.trim().equals(".")) {
            // end-of-data
            dataMode = false;
            channel.write("250 OK\r\n");
        }
        else {
            // unescape leading dot
            if (line.startsWith("..")) {
                line = line.substring(1);
            }
            message.append(line);
        }
    }
    
    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, ExceptionEvent e) 
    {
        e.getCause().printStackTrace();

        Channel ch = e.getChannel();
        ch.close();
    }
}

