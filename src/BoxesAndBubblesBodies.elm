module BoxesAndBubblesBodies (Body,Shape) where
{-| # Boxes and Bubbles Bodies.
Defines bodies as used by the Boxes and Bubbles engine. You will need these data types to 
display and modify bodies being calculated. For creating them, you may prefer the constructor 
functions in the BoxesAndBubbles module.

@docs Body, Shape

-}

import Math2D (..)

{-| A rigid body in the Boxes and Bubbles universe, as used internally by the engine.
Mass is stored as inverse, because it is more convenient for calculation.

Type parameter `a` can be used to extend bodies with arbitrary other information used
by your application. For example: label, hit points, an object type ADT, or more low-level, 
an id used to associate the body with arbitrary other data via a Dict.
-}
type Body a = { a |
  pos: Vec2, -- reference position (center)
  velocity: Vec2, -- direction and speed
  inverseMass: Float, -- we usually use only inverse mass for calculations
  restitution: Float, -- bounciness factor
  shape: Shape
}

{-| Shape data for a body. 
A bubble is defined by its radius.
A box is defined by its extents (half-width/half-height from the center).
We use half-lengths because that's what is convenient for calculation, and it's most consistent
with using radius for circles.
-}
data Shape = 
    Box Vec2 -- vector of extents (half-widths)
  | Bubble Float -- radius