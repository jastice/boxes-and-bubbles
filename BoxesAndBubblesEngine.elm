module BoxesAndBubblesEngine where
-- based roughly on http://gamedevelopment.tutsplus.com/tutorials/gamedev-6331

import Math2D (..)

-- plain old pair for coordinates, vectors
type Vec2 = (Float,Float)

type Body = {
  pos: Vec2, -- position reference
  velocity: Vec2, -- direction and speed
  inverseMass: Float, -- we usually use only inverse mass for calculations
  restitution: Float, -- bounciness factor
  shape: Shape
}

type BubbleShape = { radius: Float }
-- extents are half width/height from the center
type BoxShape = { extents: Vec2 }

data Shape = Box BoxShape | Bubble BubbleShape


-- collision calculation for different types of bodies

type CollisionResult = { normal: Vec2, penetration: Float }

-- calculate collision normal, penetration depth of a collision among bubbles
-- takes distance vector b0b1 and the bubbles as argument
-- simple optimization: doesn't compute sqrt unless necessary
collisionBubbleBubble: Vec2 -> BubbleShape -> BubbleShape -> CollisionResult
collisionBubbleBubble b0b1 b0 b1 = 
  let
    radiusb0b1 = b0.radius+b1.radius
    distanceSq = lenSq b0b1
  in
    if | distanceSq == 0 -> { normal = (1,0), penetration = b0.radius } -- same position, arbitrary normal
       | distanceSq >= radiusb0b1*radiusb0b1 -> { normal = (1,0), penetration = 0 } -- no intersection, arbitrary normal
       | otherwise -> 
          let d = sqrt distanceSq
          in { normal = div2 b0b1 d, penetration = radiusb0b1 - d }

-- takes positions and vector and extension half-lengths of boxes
collisionBoxBox: (Vec2,BoxShape) -> (Vec2,BoxShape) -> CollisionResult
collisionBoxBox (pos0,b0) (pos1,b1) =
  let n = minus pos1 pos0 |> abs2 -- distance between box centerpoints
      (ox,oy) = minus (plus b0.extents b1.extents) n -- overlaps

  in if ox > 0 
     then CollisionResult (0,0) 0
     else if ox > oy then
          if nx < 0 then CollisionResult (-1,0) ox
                    else CollisionResult (0,0) ox
     else if ny < 0 then CollisionResult (0,-1) oy
                    else CollisionResult (0,1) oy

collisionBoxBubble: BoxShape -> BubbleShape -> CollisionResult
collisionBoxBubble box bubble = {normal = (0,0), penetration = 0}

collision: Body -> Body -> CollisionResult
collision body0 body1 = case (body0.shape, body1.shape) of
  (Bubble b0, Bubble b1) -> 
    let b0b1 = minus body1.pos body0.pos
    in collisionBubbleBubble b0b1 b0 b1
  (Box b0, Box b1) -> collisionBoxBox (body0.pos,b0) (body1.pos,b1)
  (Box box, Bubble bubble) -> collisionBoxBubble box bubble
  (Bubble bubble, Box box) -> collisionBoxBubble box bubble


-- modify bodies' trajectories when they collide
resolveCollision: CollisionResult -> Body -> Body -> (Body, Body)
resolveCollision {normal,penetration} b0 b1 = 
  let 
    relativeVelocity = minus b1.velocity b0.velocity
    velocityAlongNormal = dot relativeVelocity normal
  in 
    if penetration == 0 || velocityAlongNormal > 0 then (b0,b1) -- no collision or velocities separating
    else let
      restitution = min b0.restitution b1.restitution -- collision restitution
      invMassSum = (b0.inverseMass + b1.inverseMass)
      j = (-(1 + restitution) * velocityAlongNormal) / invMassSum -- impulse scalar
      impulse = mul2 normal j
    in ({ b0 | velocity <- minus b0.velocity (mul2 impulse b0.inverseMass) },
        { b1 | velocity <- plus b1.velocity (mul2 impulse b1.inverseMass) })


-- collide a0 with all the bubbles, modifying b along the way.
-- return (updated a0, [updated bubbles])
collideWith: Body -> [Body] -> [Body] -> [Body]
collideWith a0 bodies acc = case bodies of
  [] -> a0 :: acc
  (b0 :: bs) -> 
    let collisionResult = collision a0 b0
        (a1,b1) = resolveCollision collisionResult a0 b0
    in collideWith a1 bs (b1 :: acc)

-- recursive collision resolution
collide: [Body] -> [Body] -> [Body]
collide acc bodies = 
  case bodies of
    [] -> acc
    h::t -> 
      let (h1 :: t1) = collideWith h t []
      in collide (h1::acc) t1

-- update body position with its speed and apply an additional force
update: Vec2 -> Body -> Body
update (x,y) body = { body | pos <- plus body.pos body.velocity }

-- applies accellerating force, does movement and resolves collisions for all the bubbles
step: Vec2 -> [Body] -> [Body]      
step force bodies = 
  map (update force) (collide [] bodies)
  -- resolve all collisions; optimization: broad phase
  -- TODO apply forces


