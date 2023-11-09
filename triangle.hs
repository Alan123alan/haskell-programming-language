data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

legalTriangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
legalTriangleType a b c
  |a == b && b == c = Equilateral
  |a /= b && b /= c && a /= c = Scalene
  |a == b || b == c || a == c = Isosceles

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c 
  | a == 0 && b == 0 && c == 0 = Illegal
  | a + b >= c && b + c >= a && a + c >= b = legalTriangleType a b c
  | otherwise = Illegal
