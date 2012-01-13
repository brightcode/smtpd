/**
 * Basic SMTP server in Java using Netty
 *
 * Author: Maarten Oelering
 */

import static org.jboss.netty.channel.Channels.*;

import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.handler.codec.frame.DelimiterBasedFrameDecoder;
import org.jboss.netty.handler.codec.frame.Delimiters;
import org.jboss.netty.handler.codec.string.StringDecoder;
import org.jboss.netty.handler.codec.string.StringEncoder;

public class SmtpServerPipelineFactory implements ChannelPipelineFactory 
{
    @Override
    public ChannelPipeline getPipeline() throws Exception 
    {
        // Create a default pipeline implementation.
        ChannelPipeline pipeline = pipeline();

        // Decoder that splits the received ChannelBuffers by one or more line delimiters (\r, \n)
        pipeline.addLast("framer", new DelimiterBasedFrameDecoder(
                1000, Delimiters.lineDelimiter()));
        // Decodes a received ChannelBuffer into a String
        pipeline.addLast("decoder", new StringDecoder());  // CharsetUtil.US-ASCII

        // Encodes the requested String into a ChannelBuffer
        pipeline.addLast("encoder", new StringEncoder());

        // and then business logic.
        pipeline.addLast("handler", new SmtpServerHandler());

        return pipeline;
    }
}
