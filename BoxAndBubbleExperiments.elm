import Mouse
import BoxesAndBubbles (..)

inf = 1/0

someBubbles = [ 
  makeBubble 30 (-80,0) (1.5,0),
  makeBubble2 70 (80,0) (0,0) inf 1,
  makeBubble 40 (0,200) (0.4,-3.0),
  makeBubble2 90 (400,-300) (-2,0) 0.1 0.9,
  makeBubble2 10 (300,300) (-4,-4) 1 1,
  makeBubble 40 (400,200) (-5,-1)
  ]

plain = outlined (dotted red)

drawBody {pos,velocity,inverseMass,restitution,shape} = 
  case shape of
    Bubble {radius} ->
      group [
        circle radius |> outlined (solid black),
        segment (0,0) (mul2 velocity 5) |> traced (solid red),
        ["e = ", show restitution, "\nm = ", show (round (1/inverseMass))] |> join " "
          |> toText |> centered |> toForm |> move (0,radius+8)
        ] |> move pos 
    Box a -> group [] 

scene bodies = 
  let drawnBodies = map drawBody bodies 
  in collage 800 800 drawnBodies

tick = (\t -> (0,0)) <~ every (40*millisecond)
tick2 = (\t -> (0,0)) <~ Mouse.clicks

main = scene <~ foldp step someBubbles tick 