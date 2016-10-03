# A Primitive implementation of Propagarion Networks

![CircleCi Build Status](https://circleci.com/gh/ptek/data-propagate.png?circle-token=b9ae651cb77c1d3bd3af2b37dea4afe8bb1e729f)

The idea is to have a data propagation system that is persistent and
won't lose the state between the program runs. Even if a program
crashes, it should pick up where it left off after a restart and
recompute the targets autonomously.

Having said that, the persistense layer is not implemented. The Idea
is to find out a stable API first, and then, having the tests in
place, change the inner workings of it.

The work is inspired by Alexey Andreyevich
Radul's
["Propagation Networks"](http://web.mit.edu/~axch/www/phd-thesis.pdf).
I hope it to become a practical as well as theoretically sound and
complete system. Feel free to contribute.

ITS A WORK IN PROGRESS AND IN AN EXPERIMENTAL STATE. EVERYTHING
PRESENTED HERE IS SUBJECT TO CHANGE.
