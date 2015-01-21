module BoxesAndBubblesEngine (update, collide) where
-- based loosely on http://gamedevelopment.tutsplus.com/tutorials/gamedev-6331

--import List (..)
--import Math2D (..)
import Array (..)
import Math.Vector2 (Vec2, vec2, add, sub, lengthSquared, normalize, scale, toTuple, fromTuple, getX, getY, dot)
import BoxesAndBubblesBodies (Body, Shape(..))

-- collision calculation for different types of bodies

type alias CollisionResult = { normal: Vec2, penetration: Float }

-- calculate collision normal, penetration depth of a collision among bubbles
-- takes distance vector b0b1 and the bubble radii as argument
collisionBubbleBubble: Vec2 -> Float -> Float -> CollisionResult
collisionBubbleBubble b0b1 radius0 radius1 =
  let
    radiusb0b1 = radius0 + radius1
    distanceSq = lengthSquared b0b1 -- simple optimization: doesn't compute sqrt unless necessary
  in
    if | distanceSq == 0 -> CollisionResult (vec2 1 0) radius0 -- same position, arbitrary normal
       | distanceSq >= radiusb0b1 * radiusb0b1 -> CollisionResult (vec2 1 0) 0 -- no intersection, arbitrary normal
       | otherwise ->
          let d = sqrt distanceSq
          in CollisionResult (scale (1 / d) b0b1) (radiusb0b1 - d)

-- collide two boxes
-- takes positions vector and extension half-lengths of boxes
collisionBoxBox: (Vec2,Vec2) -> (Vec2,Vec2) -> CollisionResult
collisionBoxBox (pos0,extents0) (pos1,extents1) =
  let dist = pos1 `sub` pos0 -- vector between box centerpoints
      (nx,ny) = toTuple dist
      abs2 vector = vec2 (abs <| getX vector) (abs <| getY vector)
      (ox,oy) = toTuple <| (extents0 `add` extents1) `sub` (abs2 dist) -- overlaps
   in if ox > 0 && oy > 0 then
        if ox < oy then
             if nx < 0 then CollisionResult (vec2 -1 0) ox
                       else CollisionResult (vec2 1 0) ox
        else if ny < 0 then CollisionResult (vec2 0 -1) oy
                       else CollisionResult (vec2 0 1) oy
      else CollisionResult (vec2 1 0) 0

-- collide a box with a bubble
-- takes position and half-length of box, position and radius of bubble
collisionBoxBubble: (Vec2,Vec2) -> (Vec2,Float) -> CollisionResult
collisionBoxBubble (posBox,boxExtents) (posBubble,bubbleRadius) =
  let dist = posBubble `sub` posBox
      (dx,dy) = toTuple dist
      (boxX,boxY) = toTuple boxExtents
      c = (vec2 (clamp -boxX boxX dx) (clamp -boxY boxY dy)) -- closest point on box to center of bubble
      (cx,cy) = toTuple c
      (closest,inside) =
        if dist /= c then (c,False) --circle is outside
        else -- circle is inside, clamp center to closest edge
          if abs dx > abs dy then
            if cx > 0 then ((vec2 boxX cy),True) else ((vec2 -boxX cy),True)
          else
            if cy > 0 then ((vec2 cx boxY),True) else ((vec2 cx -boxY),True)
      normal = dist `sub` closest
      normalLenSq = lengthSquared normal
   in if normalLenSq > bubbleRadius * bubbleRadius && (not inside) then CollisionResult (vec2 1 0) 0
      else let penetration =  bubbleRadius + sqrt normalLenSq
            in if inside then CollisionResult (scale -1 (normalize normal)) penetration
                         else CollisionResult (normalize normal) penetration


-- figure out what collision resolution to use
collision: Body a -> Body a -> CollisionResult
collision body0 body1 = case (body0.shape, body1.shape) of
  (Bubble b0, Bubble b1) ->
    let b0b1 = body1.pos `sub` body0.pos
    in collisionBubbleBubble b0b1 b0 b1
  (Box b0, Box b1) ->
    collisionBoxBox (body0.pos, b0) (body1.pos, b1)
  (Box box, Bubble bubble) ->
    collisionBoxBubble (body0.pos, box) (body1.pos, bubble)
  (Bubble bubble, Box box) ->
    let res = collisionBoxBubble (body1.pos, box) (body0.pos, bubble)
    -- negate the normal because the bodies were put in switched relative to their poisition in the list
    in { res | normal <- scale -1 res.normal }


-- modify bodies' trajectories when they collide
resolveCollision: CollisionResult -> Body a -> Body a -> (Body a, Body a)
resolveCollision {normal,penetration} b0 b1 =
  let
    relativeVelocity = b1.velocity `sub` b0.velocity
    velocityAlongNormal = relativeVelocity `dot` normal
  in
    if penetration == 0 || velocityAlongNormal > 0 then (b0,b1) -- no collision or velocities separating
    else let
      restitution = min b0.restitution b1.restitution -- collision restitution
      invMassSum = (b0.inverseMass + b1.inverseMass)
      j = (-(1 + restitution) * velocityAlongNormal) / invMassSum -- impulse scalar
      impulse = scale j normal -- impulse vector
    in ({ b0 | velocity <- b0.velocity `sub` (scale b0.inverseMass impulse) },
        { b1 | velocity <- b1.velocity `add` (scale b1.inverseMass impulse) })


-- collide a0 with all the bodies, modifying b along the way.
-- third argument is accumulator to make it tail recursive, even though Elm doesn't support TCO currently
-- return (updated a0, [updated bodies])
collideWith: Body a -> Array (Body a) -> Array (Body a) -> Array (Body a)
collideWith a0 bodies acc =
  case get 0 bodies of
    Nothing -> push a0 acc
    Just b0 ->
      let collisionResult = collision a0 b0
          (a1, b1) = resolveCollision collisionResult a0 b0
          bs = slice 1 (length bodies) bodies
      in collideWith a1 bs (push b1 acc)

{-}
collideWith a0 bodies acc = case bodies of
  [] -> a0 :: acc
  (b0 :: bs) ->
    let collisionResult = collision a0 b0
        (a1,b1) = resolveCollision collisionResult a0 b0
    in collideWith a1 bs (b1 :: acc)
-}
-- recursive collision resolution
collide: Array (Body a) -> Array (Body a) -> Array (Body a)
collide acc bodies =
  case get 0 bodies of
    Nothing -> acc
    Just h ->
      let t = slice 1 (length bodies) bodies
          collideWithResult = collideWith h t empty
          t1 = slice 1 (length collideWithResult) collideWithResult
      in
        case get 0 collideWithResult of
          Nothing -> acc
          Just h1 -> collide (push h1 acc) t1

{-}
collide acc bodies =
  case bodies of
    [] -> acc
    h::t ->
      let (h1 :: t1) = collideWith h t []
      in collide (h1::acc) t1
-}


-- update body position with its speed and apply additional forces
update: Vec2 -> Vec2 -> Body a -> Body a
update gravity force body =
  let accelGravity = if body.inverseMass == 0 then (vec2 0 0) else gravity
      acceleration = scale body.inverseMass force -- f = ma => a = f/m
      velocityNew = (body.velocity `add` acceleration) `add` accelGravity
      posNew = body.pos `add` body.velocity
  in { body | pos <- posNew, velocity <- velocityNew }
