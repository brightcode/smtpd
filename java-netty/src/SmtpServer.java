/**
 * Basic SMTP server in Java using Netty
 *
 * Author: Maarten Oelering
 */

import java.net.InetSocketAddress;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ServerBootstrap;
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory;

public class SmtpServer 
{
    private static int port = 2525;
    
    public static void main(String[] args)
    {
        // Configure the server.
        ServerBootstrap bootstrap = new ServerBootstrap(
                new NioServerSocketChannelFactory(
                        Executors.newCachedThreadPool(),
                        Executors.newCachedThreadPool()));

        // Configure the pipeline factory.
        bootstrap.setPipelineFactory(new SmtpServerPipelineFactory());

        //bootstrap.setOption("backlog", backlog);
        //bootstrap.setOption("tcpNoDelay", true);
        bootstrap.setOption("reuseAddress", true);
        //bootstrap.setOption("child.reuseAddress", true);
        //bootstrap.setOption("child.tcpNoDelay", true);

        // Bind and start to accept incoming connections.
        bootstrap.bind(new InetSocketAddress(port));
    }
}
