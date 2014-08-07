import Mouse
import BoxesAndBubblesEngine (..)
import BoxesAndBubbles (..)
import Math2D (mul2)

inf = 1/0

someBubbles = [ 
  basicBubble 30 (-80,0) (1.5,0),
  bubble 70 (80,0) (0,0) inf 1,
  basicBubble 40 (0,200) (0.4,-3.0),
  bubble 90 (400,-300) (-2,0) 0.1 0.9,
  bubble 10 (300,300) (-4,-4) inf 1,
  basicBubble 40 (400,200) (-5,-1)
  ]

someBoxes = [
--box: (w,h) pos velocity density restitution 
  box (100,100) (0,0) (0,0) 1 1,
  box (20,20) (-200,0) (3,0) 1 1,
  box (20,40) (200,200) (-1,-1) 1 1,
  box (160,160) (0,-200) (0,2) 1 1,
  box (60,60) (40,220) (-0.3,-2) 1 1
  ]

plain = outlined (dotted red)

drawBody {pos,velocity,inverseMass,restitution,shape} = 
  case shape of
    Bubble {radius} ->
      group [
        circle radius |> outlined (solid black),
        segment (0,0) (mul2 velocity 5) |> traced (solid red),
        ["e = ", show restitution, "\nm = ", show (round (1/inverseMass))] |> join " "
          |> toText |> centered |> toForm |> move (0,radius+16)
        ] |> move pos 
    Box {extents} -> 
      let (w,h) = extents
      in group [
        rect (w*2) (h*2) |> outlined (solid black),
        segment (0,0) (mul2 velocity 5) |> traced (solid red)
      ] |> move pos 

scene bodies = 
  let drawnBodies = map drawBody bodies 
  in collage 800 800 drawnBodies

force t = (0,0)
tick = force <~ every (40*millisecond)
tick2 = force <~ Mouse.clicks

main = scene <~ foldp step someBoxes tick
