import BoxesAndBubblesBodies (..)
import BoxesAndBubbles (..)
import Array (..)
import Math.Vector2 (vec2, toTuple, scale)
import Graphics.Collage (Form, move, filled, segment, traced, solid, toForm, group, circle, outlined, rect, collage)
import Graphics.Element (..)
import Color (..)
import Text (fromString, centered)
import Signal ((<~), foldp)
import AnimationFrame
import String

inf = 1/0 -- infinity, hell yeah
e0 = 0.8 -- default restitution coefficient

-- box: (w,h) pos velocity density restitution
-- bubble: radius pos velocity density restitution

someBodies = fromList [
  bubble 30 1 e0 (vec2 -80 0) (vec2 1.5 0),
  bubble 70 inf 0 (vec2 80 0) (vec2 0 0) ,
  bubble 40 1 e0 (vec2 0 200) (vec2 0.4 -3.0),
  bubble 80 0.1 e0 (vec2 300 -280) (vec2 -2 1),
  bubble 15 5 0.4 (vec2 300 300) (vec2 -4 -3),
  bubble 40 1 e0 (vec2 200 200) (vec2 -5 -1),
  box (vec2 100 100) 1 e0 (vec2 300 0) (vec2 0 0),
  box (vec2 20 20) 1 e0 (vec2 -200 0) (vec2 3 0),
  box (vec2 20 40) 1 e0 (vec2 200 -200) (vec2 -1 -1)
  ] `append` bounds (vec2 750 750) 100 e0 (vec2 0 0)

-- we'll just compute the label from the data in the body
bodyLabel restitution inverseMass =
  ["e = ", toString restitution, "\nm = ", toString (round (1 / inverseMass))] |> String.concat

-- and attach it to all the bodies
labeledBodies = map (\b -> { b | label = bodyLabel b.restitution b.inverseMass }) someBodies

-- why yes, it draws a body with label. Or creates the Element, rather
drawBody : Body {a | label : String} -> Form
drawBody {pos,velocity,inverseMass,restitution,shape,label} =
  let veloLine = segment (0,0) (toTuple <| scale 5 velocity) |> traced (solid red)
      info = label |> fromString |> centered |> toForm

      ready = case shape of
        Bubble radius ->
          group [
            circle radius |> outlined (solid black),
            info |> move (0,radius+16),
            veloLine
            ]
        Box extents ->
          let (w,h) = toTuple extents
          in group [
            rect (w * 2) (h * 2) |> outlined (solid black),
            info |> move (0, h + 16),
            veloLine
          ]
  in move (toTuple pos) ready

scene : Array (Body {a | label : String}) -> Element
scene bodies = collage 800 800 <| toList <| map drawBody bodies

-- different force functions to experiment with
constgravity t = ((vec2 0 -0.2), (vec2 0 0)) -- constant downward gravity
sinforce t = (vec2 ((sin <| radians (t/1000)) * 50) 0) -- sinuoidal sideways force
counterforces t = ((vec2 0 -0.01), (vec2 0 (t/1000))) -- slowly accellerating upward drift

tick = constgravity <~ foldp (+) 0 AnimationFrame.frame

main = scene <~ run labeledBodies tick
