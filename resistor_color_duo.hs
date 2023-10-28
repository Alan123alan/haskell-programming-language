data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Enum, Bounded)

resistorTensDigit :: Color -> Int
resistorTensDigit color = (resistorUnitsDigit color) * 10

resistorUnitsDigit :: Color -> Int
resistorUnitsDigit Black = 0
resistorUnitsDigit Brown = 1
resistorUnitsDigit Red = 2
resistorUnitsDigit Orange = 3
resistorUnitsDigit Yellow = 4
resistorUnitsDigit Green = 5
resistorUnitsDigit Blue = 6
resistorUnitsDigit Violet = 7
resistorUnitsDigit Grey = 8
resistorUnitsDigit White = 9



value :: (Color, Color) -> Int
value (a, b) = (resistorTensDigit a) + (resistorUnitsDigit b)
