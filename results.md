### Go

    $ time smtp-source -c -d -l 32000 -m 10000 -N -s 30 127.0.0.1:2525
    10000

    real	0m2.763s
    user	0m0.198s
    sys	0m0.914s

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

