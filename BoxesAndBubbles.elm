module BoxesAndBubbles where

import BoxesAndBubblesEngine (..)

-- constructors

-- basic bubble with some defaults
basicBubble: Float -> Vec2 -> Vec2 -> Body
basicBubble radius pos velocity = 
  bubble radius pos velocity 1 1

-- fully specified bubble
bubble: Float -> Vec2 -> Vec2 -> Float -> Float -> Body
bubble radius pos velocity density restitution = { 
  pos = pos,
  velocity = velocity, 
  inverseMass = 1/(pi*radius*radius*density), 
  restitution = restitution,
  shape = Bubble (BubbleShape radius)
  }

box: Vec2 -> Vec2 -> Vec2 -> Float -> Float -> Body
box (w,h) pos velocity density restitution = {
  pos = pos,
  velocity = velocity,
  inverseMass = 1/(w*h*density),
  restitution = restitution,
  shape = Box (BoxShape (w/2,h/2))
  }

-- updates bodies with the signal
run: Vec2 -> [Body] -> Signal a -> Signal [Body]
run gravity bodies tick = 
  let force t = gravity
  in foldp step bodies (force <~ tick)