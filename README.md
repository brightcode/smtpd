# SMTPD

Implementation of a basic SMTP daemon in various languages and platforms.

I created this project as an exercise in polyglot programming and to test the performance of various frameworks. A lot of inspiration for this project came from [HelloD](https://github.com/carbonfive/hellod).

All implementations have roughly the same functionality, which is at the "hello world" level. They listen on all interfaces for connections on port 2525 and blackhole incoming emails. For a more complete implementation of an SMTP server, you need to look for other open source projects.

## Running

### Go

    cd go
    gomake
    ./smtpd

### Node

    cd node
    node smtpd.js

### Ruby - Cool.io

    cd ruby-coolio
    bundle
    ruby smtpd.rb

### Ruby - EventMachine

    cd ruby-em
    bundle
    ruby smtpd.rb

## Performance testing

Performance testing with smtpsource. This program is distributed with Postfix.

    time smtp-source -c -d -l 32000 -m 10000 -N -s 30 127.0.0.1:2525

See results.md for test results from my MacBook Pro with 2.2 GHz Intel Core i7.
