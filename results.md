## Test results

The following command is used to test the performance:

    $ time smtp-source -c -d -l 32000 -m 10000 -N -s 30 127.0.0.1:2525

This command is run two times, the first time to "warm-up", the second time to measure.

Note that "user" and "sys" times relate to the smtp-source process, and not the smtpd process.

### Erlang

    real	0m2.693s
    user	0m0.291s
    sys	0m1.239s

### Go

    real	0m2.763s
    user	0m0.198s
    sys	0m0.914s

### Java - Netty

    real	0m1.998s
    user	0m0.252s
    sys	0m0.742s

### JRuby - Netty

    real	0m7.095s
    user	0m0.342s
    sys	0m1.419s

### Node

    real	0m6.114s
    user	0m0.196s
    sys	0m1.059s

### Ruby - Coolio

    real	0m9.006s
    user	0m0.233s
    sys	0m1.022s

### Ruby - EventMachine

    real	0m7.282s
    user	0m0.175s
    sys	0m0.587s

### JRuby - Celluloid:IO

    real	0m7.197s
    user	0m0.231s
    sys	0m1.377s

### JRuby - Celluloid actors

    real	0m3.370s
    user	0m0.312s
    sys	0m1.595s
    