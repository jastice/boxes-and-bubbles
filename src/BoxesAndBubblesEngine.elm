module BoxesAndBubblesEngine (update) where
-- based roughly on http://gamedevelopment.tutsplus.com/tutorials/gamedev-6331

import Math2D (..)
import BoxesAndBubblesBodies (..)

-- collision calculation for different types of bodies

type CollisionResult = { normal: Vec2, penetration: Float }

-- calculate collision normal, penetration depth of a collision among bubbles
-- takes distance vector b0b1 and the bubble shapes as argument
collisionBubbleBubble: Vec2 -> Float -> Float -> CollisionResult
collisionBubbleBubble b0b1 radius0 radius1 = 
  let
    radiusb0b1 = radius0 + radius1
    distanceSq = lenSq b0b1 -- simple optimization: doesn't compute sqrt unless necessary
  in
    if | distanceSq == 0 -> CollisionResult (1,0) radius0 -- same position, arbitrary normal
       | distanceSq >= radiusb0b1*radiusb0b1 -> CollisionResult (1,0) 0 -- no intersection, arbitrary normal
       | otherwise -> 
          let d = sqrt distanceSq
          in CollisionResult (div2 b0b1 d) (radiusb0b1 - d)

-- collide two boxes
-- takes positions vector and extension half-lengths of boxes
collisionBoxBox: (Vec2,Vec2) -> (Vec2,Vec2) -> CollisionResult
collisionBoxBox (pos0,extents0) (pos1,extents1) =
  let dist = minus pos1 pos0 -- vector between box centerpoints
      (nx,ny) = dist
      (ox,oy) = minus (plus extents0 extents1) (abs2 dist) -- overlaps
   in if ox > 0 && oy > 0 then
        if ox < oy then
             if nx < 0 then CollisionResult (-1,0) ox
                       else CollisionResult (1,0) ox
        else if ny < 0 then CollisionResult (0,-1) oy
                       else CollisionResult (0,1) oy
      else CollisionResult (1,0) 0

-- collide a box with a bubble
-- takes position and half-length of box, position and radius of bubble
collisionBoxBubble: (Vec2,Vec2) -> (Vec2,Float) -> CollisionResult
collisionBoxBubble (posBox,boxExtents) (posBubble,bubbleRadius) = 
  let dist = minus posBubble posBox
      (dx,dy) = dist
      (boxX,boxY) = boxExtents
      c = (clamp -boxX boxX dx, clamp -boxY boxY dy) -- closest point on box to center of bubble
      (cx,cy) = c
      (closest,inside) = 
        if dist /= c then (c,False) --circle is outside
        else -- circle is inside, clamp center to closest edge
          if abs dx > abs dy then
            if cx > 0 then ((boxX,cy),True) else ((-boxX,cy),True)
          else
            if cy > 0 then ((cx,boxY),True) else ((cx,-boxY),True)
      normal = minus dist closest
      normalLenSq = lenSq normal
   in if normalLenSq > bubbleRadius*bubbleRadius && (not inside) then CollisionResult (1,0) 0
      else let penetration =  bubbleRadius + sqrt normalLenSq
            in if inside then CollisionResult (mul2 (norm normal) -1) penetration
                         else CollisionResult (norm normal) penetration


-- figure out what collision resolution to use
collision: Body a -> Body a -> CollisionResult
collision body0 body1 = case (body0.shape, body1.shape) of
  (Bubble b0, Bubble b1) -> 
    let b0b1 = minus body1.pos body0.pos
    in collisionBubbleBubble b0b1 b0 b1
  (Box b0, Box b1) ->
    collisionBoxBox (body0.pos, b0) (body1.pos, b1)
  (Box box, Bubble bubble) ->
    collisionBoxBubble (body0.pos, box) (body1.pos, bubble)
  (Bubble bubble, Box box) ->
    let res = collisionBoxBubble (body1.pos, box) (body0.pos, bubble)
    -- negate the normal because the bodies were put in switched relative to their poisition in the list
    in { res | normal <- neg res.normal }


-- modify bodies' trajectories when they collide
resolveCollision: CollisionResult -> Body a -> Body a -> (Body a, Body a)
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
collideWith: Body a -> [Body a] -> [Body a] -> [Body a]
collideWith a0 bodies acc = case bodies of
  [] -> a0 :: acc
  (b0 :: bs) -> 
    let collisionResult = collision a0 b0
        (a1,b1) = resolveCollision collisionResult a0 b0
    in collideWith a1 bs (b1 :: acc)

-- recursive collision resolution
collide: [Body a] -> [Body a] -> [Body a]
collide acc bodies = 
  case bodies of
    [] -> acc
    h::t -> 
      let (h1 :: t1) = collideWith h t []
      in collide (h1::acc) t1

-- update body position with its speed and apply additional forces
update: Vec2 -> Vec2 -> Body a -> Body a
update gravity force body = 
  let accelGravity = if body.inverseMass == 0 then (0,0) else gravity
      acceleration = mul2 force body.inverseMass -- f = ma => a = f/m
      velocityNew = plus accelGravity <| plus body.velocity acceleration
      posNew = plus body.pos body.velocity
  in { body | pos <- posNew, velocity <- velocityNew }




