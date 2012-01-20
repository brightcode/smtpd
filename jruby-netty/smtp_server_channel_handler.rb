#
# Basic SMTP server in JRuby using Netty
#
# Author: Maarten Oelering
#

require 'socket'
require "java"
require "netty-3.2.6.Final.jar"  # must be in classpath or load path

java_import org.jboss.netty.channel.Channel;
java_import org.jboss.netty.channel.ChannelEvent;
java_import org.jboss.netty.channel.ChannelFuture;
java_import org.jboss.netty.channel.ChannelFutureListener;
java_import org.jboss.netty.channel.ChannelHandlerContext;
java_import org.jboss.netty.channel.ChannelStateEvent;
java_import org.jboss.netty.channel.ExceptionEvent;
java_import org.jboss.netty.channel.MessageEvent;
java_import org.jboss.netty.channel.SimpleChannelUpstreamHandler;
java_import org.jboss.netty.channel.ChannelFuture;
java_import org.jboss.netty.channel.ChannelFutureListener;

class SmtpServerChannelHandler < SimpleChannelUpstreamHandler
  # implements
  #include org.jboss.netty.channel.Channel
  #include org.jboss.netty.channel.ChannelHandler
  
  @@hostname = Socket.gethostname
  
  def initialize
    @state = :null
  end
  
  public
  def channelConnected(context, channel_state_event)
    channel = channel_state_event.getChannel()
    channel.write("220 #{@@hostname} ESMTP\r\n")
  end
  
  public
  def messageReceived(context, message_event)
    channel = message_event.getChannel()
    line = message_event.getMessage()
    if @state == :data
      if line == "."
        @state = :end_of_data
        channel.write "250 OK\r\n"
      else
        line.slice!(0...1) if line[0] == ?.
        @message << line
      end
    else
      # split command and parameters at first <SP>
      command, params = line.split(' ', 2)
      
      case command.upcase
      when 'HELO'
        channel.write "250 #{@@hostname}\r\n"
      when 'EHLO'
        channel.write "250 #{@@hostname}\r\n"
      when 'MAIL'
        @message = []
        channel.write "250 OK\r\n"
      when 'RCPT'
        channel.write "250 OK\r\n"
      when 'DATA'
        @state = :data
        channel.write "354 End data with <CR><LF>.<CR><LF>\r\n"
      when 'RSET'
        channel.write "250 OK\r\n"
      when 'QUIT'
        future = channel.write("221 #{@@hostname} closing connection\r\n");
        future.addListener(ChannelFutureListener.CLOSE);
      else
        channel.write "500 unrecognized command\r\n"
      end
    end
  end
  
  public
  def exceptionCaught(context, exception_event) 
    #    e.getCause().printStackTrace();
    #
    #    Channel ch = e.getChannel();
    #    ch.close();
    puts "Exception -- " + exception_event.to_s
    puts "Backtrace: " + exception_event.cause.getStackTrace
  end
  
end

