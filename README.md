Boxes and Bubbles
=================


A simple-as-possible 2D physics rigid-body physics engine for Elm.
Supports only bubbles (circles) and boxes (rectangles).

It does this:

* resolve collisions between bodies of different mass and bounciness.
* gravity / global forces

It doesn't do:

* arbitrary polygons
* friction
* rotation
* time-integrated movement
* graphics
* colliding unstoppable forces with immovable objects (infinite masses will be glitchy)
