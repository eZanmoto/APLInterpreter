APL Interpreter
===============

Running
-------

To prepare your environment to compile Scala with ant, please ensure that
`scala-library.jar` and `scala-compiler.jar` are in `$SCALA_HOME/lib`, and that
`$SCALA_HOME` is set accordingly.

To run, execute

    ant

to quit, run the command

    )OFF

Example Session
---------------

    CLEAR WS

          A : 10
          A
    10
          B : 5

          A + B
    15

          1 2 3 4
    1 2 3 4

          1 2 3 4 + 1
    2 3 4 5

          1 2 3 4 + 1 2 3 4
    2 4 6 8

        L : 1 2
        L : L, 3 4
        L
    1 2 3 4

        L x 2
    2 4 6 8

        L x 1 2 3 4
    1 4 9 16

        L : L x 1 2 3 4
        L [ 3 ]
    9

        L [ 3 ] : 25
        L
    1 4 25 16

        L [ 1 + 3 ]
    16

        L [ 1 ] + L [ 3 ]
    26

        L [ 1 3 ]
    1 25

        L [ 3 1 ]
    25 1

        S : 'HI'
        S [ 2 ] : 'A'
        S
    HA

        S : 'BAKA'
        S [ 1 4 ] : 'CE'
        S
    CAKE

        I : 1 2 3 4
        I [ 4 1 ] : I [ 1 4 ]
        I
    4 2 3 1

        S = 'HIKE'
    0 0 1 1

        S = 'A'
    0 1 0 0

        'A' = S
    0 1 0 0

        S = S
    1 1 1 1

        L : 1 2 3 4 5

        L = 3
    0 0 1 0 0

        L n 3
    1 1 0 1 1

        L < 3
    1 1 0 0 0

        L l 3
    1 1 1 0 0

        L > 3
    0 0 0 1 1

        L g 3
    0 0 1 1 1

        L r 3
    3 3 3 4 5

        L _ 3
    1 2 3 3 3

        p L
    5

        p L , 0
    6

        p L [ 2 3 4 ]
    3

        i 5
    1 2 3 4 5

        +/ 1 2 3
    6

        +/ i 5
    15

        d SUMAB
    [1] A + B
    [2] d

        SUMAB
    15

        )ERASE SUMAB

        SUMAB
    [!] Error: 'SUMAB' has not been declared

        )OFF
    Goodbye.

About
-----

Written by Sean Kelleher.
