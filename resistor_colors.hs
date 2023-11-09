
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
  deriving (Show, Enum, Bounded)

firstBandValue :: Color -> Int
firstBandValue color = (fromEnum color) * 10

secondBandValue :: Color -> Int
secondBandValue color = fromEnum color

multiplierValue :: Color -> Int
multiplierValue color = 10 ^ (fromEnum color)

resistanceToLabel :: Int -> String
resistanceToLabel resistance
  |resistance < 1000 = (show resistance) ++ " ohms"
  |resistance < 1000000 = (show (div resistance 1000)) ++ " kiloohms"
  |resistance < 1000000000 = (show (div resistance 1000000)) ++ " megaohms"
  |resistance < 1000000000000 = (show (div resistance 1000000000)) ++ " gigaohms"

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> String
label resistor = resistanceToLabel (ohms resistor)

ohms :: Resistor -> Int
ohms (Resistor(firstBand, secondBand, multiplier)) = ((firstBandValue firstBand) + (secondBandValue secondBand)) * (multiplierValue multiplier)
