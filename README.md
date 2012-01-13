SMTPD
====================

Implementation of a basic SMTP server in various languages and platforms.

Ruby / EventMachine
-----------

  cd ruby-em
  bundle
  ruby smtpd.rb

Ruby / Cool.io
-----------

  cd ruby-coolio
  bundle
  ruby smtpd.rb

Node.JS
-----------

  cd node
  node smtpd.js
  
Java / Netty
-----------

  cd java-netty
  ./compile
  ./smtpd

Performance testing
-----------

Performance testing with smtpsource. This program is distributed with Postfix.

time smtp-source -c -d -l 32000 -m 10000 -N -s 30 127.0.0.1:2525

See results.md for test results from my MacBook Pro with 2.2 GHz Intel Core i7.
