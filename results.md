### Erlang

    $ time smtp-source -c -d -l 32000 -m 10000 -N -s 30 127.0.0.1:2525
    10000

    real	0m2.693s
    user	0m0.291s
    sys	0m1.239s

### Go

    $ time smtp-source -c -d -l 32000 -m 10000 -N -s 30 127.0.0.1:2525
    10000

    real	0m2.763s
    user	0m0.198s
    sys	0m0.914s

### Java - Netty

    $ time smtp-source -c -d -l 32000 -m 10000 -N -s 30 127.0.0.1:2525
    10000

    real	0m1.998s
    user	0m0.252s
    sys	0m0.742s

### JRuby - Netty

    $ time smtp-source -c -d -l 32000 -m 10000 -N -s 30 127.0.0.1:2525
    10000

    real	0m7.095s
    user	0m0.342s
    sys	0m1.419s

### Node

    $ time smtp-source -c -d -l 32000 -m 10000 -N -s 30 127.0.0.1:2525
    10000

    real	0m6.114s
    user	0m0.196s
    sys	0m1.059s

### Ruby - Coolio

    $ time smtp-source -c -d -l 32000 -m 10000 -N -s 30 127.0.0.1:2525
    10000

    real	0m9.006s
    user	0m0.233s
    sys	0m1.022s

### Ruby - EventMachine

    $ time smtp-source -c -d -l 32000 -m 10000 -N -s 30 127.0.0.1:2525
    10000

    real	0m7.282s
    user	0m0.175s
    sys	0m0.587s

