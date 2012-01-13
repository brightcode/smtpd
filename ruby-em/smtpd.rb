#
# SMTP Deamon in Ruby using EventMachine
#
# Author: Maarten Oelering
#

require 'socket'
require 'eventmachine'

class SmtpServerConnection < EM::Protocols::LineAndTextProtocol

  # note: EM::Protocols::LineText2 twice as slow
  
  def initialize(*args)
    super
    @hostname = args.first
    @data_mode = false
  end
  
  def post_init
    send_data "220 #{@hostname} ESMTP\r\n"
  end

  def receive_line(line)
    return process_data_line(line) if @data_mode
    
    command = line[0...4].upcase
    case command
    when 'HELO'
      send_data "250 #{@hostname}\r\n"
    when 'EHLO'
      send_data "250-#{@hostname}\r\n250 PIPELINING\r\n"
    when 'MAIL'
      @message = []
      send_data "250 OK\r\n"
    when 'RCPT'
      send_data "250 OK\r\n"
    when 'DATA'
      @data_mode = true
      send_data "354 End data with <CR><LF>.<CR><LF>\r\n"
    when 'QUIT'
      send_data "221 #{@hostname} closing connection\r\n"
      close_connection_after_writing
    else
      send_data "500 unrecognized command\r\n"
    end
  end
  
  def process_data_line line
    if line == "."
      @data_mode = false
      send_data "250 OK\r\n"
    else
      # slice off leading . if any
      line.slice!(0...1) if line[0] == ?.
      @message << line
    end
  end
  
end

EM.run do
  Signal.trap("INT")  { EM.stop }
  Signal.trap("TERM") { EM.stop }

  EM.start_server("0.0.0.0", 2525, SmtpServerConnection, Socket.gethostname)
end

