/* 
 * Simple SMTP server in Go
 *
 * Author: Maarten Oelering
 */
package main

import (
    "fmt"
    "os"
    "net"
    "bufio"
    "strings"
)

var hostname string;

func main() {
    hostname, _ = os.Hostname()

    tcpAddress, _ := net.ResolveTCPAddr(":2525")
    listener, _ := net.ListenTCP("tcp", tcpAddress)
    for {
        conn, _ := listener.AcceptTCP()
        go handleConnection(conn)
    }
    listener.Close()
}

func handleConnection(conn *net.TCPConn) {
    defer conn.Close()

    reader := bufio.NewReader(conn)
    
    fmt.Fprintf(conn, "220 %s ESMTP\r\n", hostname)
    
    dataMode := false
    for {
        line, _ := reader.ReadString('\n')
        if ! dataMode {
            command := strings.ToUpper(line[0:4])
            switch (command) {
            case "HELO":
                fmt.Fprintf(conn, "250 %s\r\n", hostname)
            case "EHLO":
                fmt.Fprintf(conn, "250-%s\r\n250 PIPELINING\r\n", hostname)
            case "MAIL":
                conn.Write([]byte("250 OK\r\n"))
            case "RCPT":
                conn.Write([]byte("250 OK\r\n"))
            case "DATA":
                dataMode = true
                conn.Write([]byte("354 End data with <CR><LF>.<CR><LF>\r\n"))
            case "QUIT":
                fmt.Fprintf(conn, "221 %s closing connection\r\n", hostname)
                return  // and close
            default:
                conn.Write([]byte("500 unrecognized command\r\n"))
            }
        } else {
            if len(line) >= 2 && line[0] == '.' && (line[1] == '\r' || line[1] == '\n') {
                dataMode = false
                conn.Write([]byte("250 OK\r\n"))
            } else {
                if strings.HasPrefix(line, "..") {
                    line = line[1:]
                }
                // TODO: collect data in var
            }
        }
    }
}
