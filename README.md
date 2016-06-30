Boxes and Bubbles
=================

[![Join the chat at https://gitter.im/jastice/boxes-and-bubbles](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/jastice/boxes-and-bubbles?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

A simple-as-possible 2D physics rigid-body physics engine for Elm.
Supports only bubbles (circles) and boxes (axis-aligned rectangles).

Here's [an example](http://jastice.github.io/boxes-and-bubbles/) ([source](https://github.com/jastice/boxes-and-bubbles/blob/master/examples/Example.elm)) of the engine in action. To run the example locally, start elm-reactor in the `examples` directory.

It does this:

* resolve collisions between bodies of different mass and bounciness.
* gravity (ignores mass) / global time-varying forces (mass-dependent)

It doesn't do:

* arbitrary polygons
* friction / drag
* rotation
* time-integrated movement
* graphics
* colliding unstoppable forces with immovable objects (infinite masses will be glitchy)
