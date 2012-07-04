#
# SMTP deamon in Ruby using Celluloid actors with blocking IO
#
# Run with:
#    jruby --1.9 smtpd.rb
#
# Author: Maarten Oelering
#

require 'rubygems'
require 'celluloid'
require 'socket'

class SmtpServerConnection
  
  include Celluloid
  
  @@hostname = Socket.gethostname
  
  def initialize(socket)
    @socket = socket
    # reset state
    @state = nil
  end

  def run
    # send banner greeting
    reply "220 #{@@hostname} ESMTP"
    while line = @socket.gets("\n", 1000)
      parse_line(line.chomp)
      break if @state == :quit
    end
  rescue EOFError  # Errno::ECONNRESET, EOFError, IOError, Errno::EBADF, Errno::EPIPE
    # peer disconnected
  ensure
    @socket.close
    terminate  # terminate this actor
  end

  #
  # intermediate helpers
  #
  
  def parse_line(line)
    if @state != :data
      parse_command(line.rstrip) # remove trailing white space
    else
      parse_data_line(line)
    end
  end
  
  def parse_command(line)
    # split command and parameters at first <SP>
    command, params = line.split(' ', 2)

    command.upcase!
    
    # dispatch command
    case command.upcase
    when 'HELO'
      on_helo(params)
    when 'EHLO'
      on_ehlo(params)
    when 'VRFY'
      on_vrfy(params)
    when 'MAIL'
      on_mail(params) # TDO remove /(from)?: ?/
    when 'RCPT'
      on_rcpt(params) # TDO remove /(to)?: ?/
    when 'DATA'
      on_data
    when 'RSET'
      on_rset
    when 'NOOP'
      on_noop
    when 'QUIT'
      on_quit
    else
      reply "500 unrecognized command"
    end
  rescue => e
    $stderr.puts e.to_s
    $stderr.puts e.backtrace.join("\n")
    reply "451 #{e.to_s}"
  end
  
  def parse_data_line(line)
    if line == "."   # .CRLF and .LF are treated as end of mail data
      on_end_of_data(@data)
    else
      # unescape ^..
      line.slice!(0...1) if line[0] == ?.
      @data << line
    end
  end
 
  #
  # handle smtp commands
  #
  
  def on_helo(domain)
    if domain.nil?
      reply "501 Syntax: HELO hostname"
      return
    end
    
    @state = :helo
    reply "250 #{@@hostname}"
  end
  
  def on_ehlo(domain)
    if domain.nil?
      reply "501 Syntax: EHLO hostname"
      return
    end
    
    @state = :helo
    reply_multi(250, @@hostname, 'PIPELINING', '8BITMIME', 'ENHANCEDSTATUSCODES')
  end

  def on_mail(params)
    if @state.nil?
      reply "503 5.5.1 Error: send HELO/EHLO first"
      return
    end
    
    # mail is allowed after helo, rset or end of data  
    if @state != :helo
      reply "503 5.5.1 Error: nested MAIL command"
      return
    end

    # first argument is FROM:

    address = parse_address(params)
    unless address
      reply "501 5.5.4 Syntax: MAIL FROM:<address>"
      return
    end
    
    # start new transaction
    @state = :mail
    @data = []
    reply "250 2.1.0 Ok"
  end

  def on_rcpt(params)
    if @state == :helo
      reply "503 5.5.1 Error: need MAIL command"
      return
    end
    
    # first argument is TO:
    
    address = parse_address(params)
    unless address
      reply "501 5.5.4 Syntax: RCPT TO:<address>"
      return
    end

    # TODO 501 5.1.3 Bad recipient address syntax  

    @state = :rcpt
    reply "250 2.1.5 Ok"
  end
  
  def on_data
    if @state != :rcpt
      reply "554 5.5.1 Error: no valid recipients"
      return
    end

    @state = :data
    reply "354 End data with <CR><LF>.<CR><LF>"
  end

  def on_end_of_data(data)
    # transaction finished
    @state = :helo
    reply "250 2.0.0 Ok"  # queued as %s
  end

  def on_rset
    # abort transaction and restore state to right after HELO/EHLO
    if (@state == :mail || @state == :rcpt || @state == :data)
      @state = :helo
    end
    reply "250 2.0.0 Ok"
  end

  def on_quit
    reply "221 #{@@hostname} closing connection"
    @state = :quit
    # close socket on write complete
    @socket
  end
  
  #
  # helpers
  #
  
  def parse_address(params)
    if params
      if address = params[/: ?<?([^> ]*)>?/, 1]
        if address.length > 0
          address
        else
          nil
        end
      end
    end
  end
    
  def reply(message)
    @socket.write(message + "\r\n")
  end

  def reply_multi(*args)
    code = args.shift.to_s # remove first element
    last = args.pop # remove last element
    resp = args.inject("") do |resp, value|
      resp << "#{code}-#{value}\r\n"
    end
    resp << "#{code} #{last}\r\n"
    @socket.write(resp)
  end
  
end

class SmtpServer
  
  def initialize(host, port)
    @server = TCPServer.new(host, port)  # Celluloid::IO::TCPServer?
  end
  
  def finalize
    @server.close if @server
  end
  
  def run
    loop do
      handle_connection @server.accept
    end
  end
  
  def handle_connection(socket)
    connection = SmtpServerConnection.new(socket)
    connection.run! # call asynchronously
  end
end

server = SmtpServer.new("127.0.0.1", 2525)
server.run
