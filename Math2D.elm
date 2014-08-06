module Math2D where

-- just vector things

plus: Vec2 -> Vec2 -> Vec2
plus (x0,y0) (x1,y1) = (x0+x1,y0+y1)

minus: Vec2 -> Vec2 -> Vec2
minus (x0,y0) (x1,y1) = (x0-x1,y0-y1)

dot: Vec2 -> Vec2 -> Float
dot (x0,y0) (x1,y1) = x0*x1 + y0*y1

div2: Vec2 -> Float -> Vec2
div2 (x,y) a = (x/a, y/a)

mul2: Vec2 -> Float -> Vec2
mul2 (x,y) a = (x*a, y*a)

abs2: Vec2 -> Vec2
abs2 (x,y) = (abs x, abs y)

-- squared norm/length of ector
lenSq: Vec2 -> Float
lenSq (x,y) = x*x + y*y