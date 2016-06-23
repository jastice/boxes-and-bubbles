module Math2D exposing (..)

-- just vector things

type alias Vec2 = (Float,Float)

plus: Vec2 -> Vec2 -> Vec2
plus (x0,y0) (x1,y1) = (x0+x1,y0+y1)

minus: Vec2 -> Vec2 -> Vec2
minus (x0,y0) (x1,y1) = (x0-x1,y0-y1)

-- element-wise vector multiplication
mul: Vec2 -> Vec2 -> Vec2
mul (x0,y0) (x1,y1) = (x0*x1, y0*y1)

dot: Vec2 -> Vec2 -> Float
dot (x0,y0) (x1,y1) = x0*x1 + y0*y1

-- vector-scalar ops
div2: Vec2 -> Float -> Vec2
div2 (x,y) a = (x/a, y/a)

mul2: Vec2 -> Float -> Vec2
mul2 (x,y) a = (x*a, y*a)

-- stuff
abs2: Vec2 -> Vec2
abs2 (x,y) = (abs x, abs y)

neg: Vec2 -> Vec2
neg (x,y) = (-x,-y)

-- squared norm/length of vector
lenSq: Vec2 -> Float
lenSq (x,y) = x*x + y*y

norm: Vec2 -> Vec2
norm v = div2 v <| sqrt (lenSq v)