#
# Basic SMTP server in JRuby using Netty
#
# Author: Maarten Oelering
#

require "java"
require "netty-3.2.6.Final.jar"  # must be in classpath or load path
require 'smtp_server_pipeline_factory'

java_import java.net.InetSocketAddress
java_import java.util.concurrent.Executors

java_import org.jboss.netty.bootstrap.ServerBootstrap;
java_import org.jboss.netty.channel.ChannelFactory
java_import org.jboss.netty.channel.ChannelPipelineFactory
java_import org.jboss.netty.channel.Channels
java_import org.jboss.netty.channel.SimpleChannelUpstreamHandler
java_import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory

class SmtpServer

  def start
    factory = NioServerSocketChannelFactory.new(Executors.newCachedThreadPool, Executors.newCachedThreadPool)
    bootstrap = ServerBootstrap.new(factory)
    bootstrap.setPipelineFactory(SmtpServerPipelineFactory.new)
    
    #bootstrap.setOption("backlog", backlog);
    bootstrap.setOption("reuseAddress", true)
    bootstrap.setOption("child.tcpNoDelay", true)
    #bootstrap.setOption("child.keepAlive", true)

    bootstrap.bind(InetSocketAddress.new(2525))
    
  end
  
end

SmtpServer.new.start
