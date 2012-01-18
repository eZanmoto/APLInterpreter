APL Interpreter
===============

Running
-------

To run, execute

    ant

to quit, run the command

    :q

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
    List(2, 3, 4, 5)

          1 2 3 4 + 1 2 3 4
    List(2, 4, 6, 8)

        L : 1 2
        L : L, 3 4
        L
    List(1, 2, 3, 4)

        L x 2
    List(2, 4, 6, 8)

        L x 1 2 3 4
    List(1, 4, 9, 16)

        L : L x 1 2 3 4
        L [ 3 ]
    9

        L [ 3 ] : 25
        L
    List(1, 4, 25, 16)

        L [ 1 + 3 ]
    16

        L [ 1 ] + L [ 3 ]
    26

        L [ 1 3 ]
    List(1, 25)

        L [ 3 1 ]
    List(25, 1)

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
    List(4, 2, 3, 1)

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

        :q
    Goodbye.

About
-----

Written by Sean Kelleher.
