-- Дописать этот текст так, чтобы написанное ниже
-- компилировалось и работало

data Point = Point (Int, Int) deriving Show

new a b = a $ b
a ->> b = b $ a

getX (Point (x, y)) = x
getY (Point (x, y)) = y

setX x' (Point (x, y)) = Point (x', y)
setY y' (Point (x, y)) = Point (x, y')

main = do
  let p0 = new Point (2, 3)  
      x  = p0->>getX
      y  = p0->>getY
      p1 = p0->>setX 8
      p2 = p1->>setY 9
  return (p2)