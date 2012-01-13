/*
 * Simple SMTP server in Node.JS
 *
 * Author: Maarten Oelering
 */
 
var net = require('net'),
    sys = require('util'),
    os = require('os');

var listenPort = 2525;
var listenAddress = "0.0.0.0";
var hostname = os.hostname();

var server = net.createServer(function (socket) {
  
  var buffer = '';
  var dataState = false;
  var content = '';
  
  socket.setEncoding('ascii');

  socket.on('connect', function () {
    socket.write('220 ' + hostname + ' ESMTP\r\n');
  });

  socket.on('data', function (data) {
    buffer += data;
    var start = 0;
    while (true) {
      // find next LF in buffer
      var i = buffer.indexOf('\n', start);
      if (i == -1) {
        // keep part after last LF in buffer
        if (start > 0)
          buffer = buffer.slice(start);
        break;
      }
      // trim trailing CR from line
      var end = (i > start && buffer.charAt(i - 1) == '\r') ? (i - 1) : i;
      // emit line without trailing (CR)LF
      socket.emit('line', buffer.substring(start, end));
      start = i + 1;
      // buffer ended with LF
      if (start == buffer.length) {
        buffer = '';
        break;
      }
    }
  });

  socket.on('line', function (line) {
    if (!dataState) {
      var command = line.substring(0, 4);
      switch (command.toUpperCase()) {
        case 'HELO':
          socket.write('250 ' + hostname + '\r\n');
          break;
        case 'EHLO':
          socket.write('250-' + hostname + '\r\n250 PIPELINING\r\n');
          break;
        case 'MAIL':
          socket.write('250 OK\r\n');
          break;
        case 'RCPT':
          socket.write('250 OK\r\n');
          break;
        case 'DATA':
          dataState = true;
          socket.write('354 End data with <CR><LF>.<CR><LF>\r\n');
          break;
        case 'RSET':
          socket.write('250 OK\r\n');
          break;
        case 'QUIT':
          // close connection after sending response
          socket.end('221 ' + hostname + ' closing connection\r\n');
          break;
        default:
          socket.write('500 unrecognized command\r\n');
          break;
      }
    }
    else {
      if (line == '.') {
        dataState = false;
        socket.write('250 OK\r\n');
      }
      else {
        // todo: unescape /^\./
        content += line;
        content += '\r\n';
      }
    }
  });
  
  socket.on('timeout', function () {
    console.log("timeout");
  });

  socket.on('end', function () {
    // do nothing
  });

  socket.on('error', function (e) {
    console.log("error:" + e);
  });
});

server.listen(listenPort, listenAddress);

