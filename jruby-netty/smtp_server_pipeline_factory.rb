#
# Basic SMTP server in JRuby using Netty
#
# Author: Maarten Oelering
#

require "java"
require "netty-3.2.6.Final.jar"  # must be in classpath or load path
require 'smtp_server_channel_handler'

java_import org.jboss.netty.channel.ChannelPipeline
java_import org.jboss.netty.channel.ChannelPipelineFactory
java_import org.jboss.netty.channel.Channels
java_import org.jboss.netty.channel.SimpleChannelUpstreamHandler
java_import org.jboss.netty.handler.codec.frame.DelimiterBasedFrameDecoder;
java_import org.jboss.netty.handler.codec.frame.Delimiters;
java_import org.jboss.netty.handler.codec.string.StringDecoder;
java_import org.jboss.netty.handler.codec.string.StringEncoder;

class SmtpServerPipelineFactory
  # implements
  include ChannelPipelineFactory
  
  public
  def getPipeline
    # Create a default pipeline implementation.
    pipeline = Channels.pipeline()
    
    # Decoder that splits the received ChannelBuffers by one or more line delimiters (\r, \n)
    pipeline.addLast("framer", DelimiterBasedFrameDecoder.new(
                1000, Delimiters.lineDelimiter()))
    # Decodes a received ChannelBuffer into a String
    pipeline.addLast("decoder", StringDecoder.new())  # CharsetUtil.US-ASCII

    # Encodes the requested String into a ChannelBuffer
    pipeline.addLast("encoder", StringEncoder.new())

    # and then business logic.
    pipeline.addLast("handler", SmtpServerChannelHandler.new)

    return pipeline;
  end
  
end

