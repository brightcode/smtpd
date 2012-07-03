
import akka.actor._
import akka.util.{ByteString, ByteStringBuilder}

import java.net.{InetAddress, InetSocketAddress}

import scala.util.matching.Regex

object SmtpServer {
  
  def main(args: Array[String]) = {
    val system = ActorSystem()
    val server = system.actorOf(Props(new SmtpServer(2525)))
    println("Press a key to stop...")
    System.in.read()
    println("Stopping...")
    system.shutdown()
  }
}

// see also https://github.com/jboner/akka/blob/master/akka-actor-tests/src/test/scala/akka/actor/IOActor.scala

class SmtpServer(port: Int) extends Actor {

  val hostname = InetAddress.getLocalHost.getHostName();
  val greeting = ByteString("220 " + hostname + "\r\n")
  
  val state = IO.IterateeRef.Map.async[IO.Handle]()(context.dispatcher)
  //val state = IO.IterateeRef.Map[IO.Handle]()
  //val state = IO.IterateeRef.Map.sync[IO.Handle]()

  val EndOfLine = ByteString("\r\n")
  val EndOfData = ByteString("\n.")
  
  val HeloCommand = new Regex("""(?i:HELO|EHLO) ([^ ]+)""")
  val MailCommand = new Regex("""(?i)MAIL FROM: ?<?([^>]*)>?.*""")
  val RcptCommand = new Regex("""(?i)RCPT TO: ?<([^>]+)>.*""")
  val DataCommand = new Regex("""(?i)DATA""")
  val QuitCommand = new Regex("""(?i)QUIT""")

  // val ioManager = Actor.actorOf(new IOManager())
  // IO.listen(ioManager, "localhost", port)
  override def preStart {
    IOManager(context.system).listen(new InetSocketAddress(port))
  }
  
  def receive = {
    case IO.Listening(server, address) =>
      println("The server is listening on socket " + address)
        
    case IO.NewClient(server) =>
      val socket = server.accept()
      state(socket) flatMap { _ =>
        IO.repeat {
          IO.take(4).map (_.decodeString("US-ASCII").toUpperCase) flatMap { 
            case "HELO" =>
              IO.takeUntil(EndOfLine) flatMap { _ =>
                socket.write(ByteString("250\r\n"))
                IO Done None
              }
            case "DATA" =>
              socket.write(ByteString("354\r\n"))
              IO.takeUntil(EndOfData) flatMap { _ =>
                socket.write(ByteString("250\r\n"))
                IO Done None
              }
            case "QUIT" =>
              IO.takeUntil(EndOfLine) flatMap { _ =>
                socket.write(ByteString("221\r\n"))
                socket.close()
                IO Done None
              }
            case _ =>
              socket.write(ByteString("550 unrecognized command\r\n"))
              IO.takeUntil(EndOfLine) flatMap {_ => IO Done None}
          }
        }
      }
      /*
      state(socket) flatMap { _ =>
        IO.repeat {
          IO.takeUntil(EndOfLine).map (_.decodeString("US-ASCII")) flatMap {
            case HeloCommand(heloname) =>
              socket.write(ByteString("250 " + hostname + "\r\n"))
              
            case MailCommand(address) =>
              socket.write(ByteString("250 OK\r\n"))
            case RcptCommand(address) =>
              socket.write(ByteString("250 OK\r\n"))
            case DataCommand() =>
              state(socket) flatMap {
                IO.takeUntil(EndOfData, true).map (_.decodeString("US-ASCII")) map {
                  s => socket.write(ByteString("250 OK " + s.length + "\r\n"))
                }
              }
              socket.write(ByteString("354 End data with <CR><LF>.<CR><LF>\r\n"))
            case QuitCommand() =>
              socket.write(ByteString("221 " + hostname + " closing connection\r\n"))
              socket.close()
            case _ =>
              socket.write(ByteString("550 unrecognized command\r\n"))
          } // map (socket write)
        }
      }
      */
      socket.write(greeting)
      
    case IO.Read(socket, bytes) =>
      state(socket)(IO Chunk bytes)
    
    case IO.Closed(socket, cause) =>
      //state(socket)(IO EOF cause)
      state -= socket
  }
}
