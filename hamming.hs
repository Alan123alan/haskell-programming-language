differences :: String -> String -> [Int]
differences xs ys = [i | i<-[0..(length xs)-1], xs!!i /= ys!!i]

distance :: String -> String -> Maybe Int
distance xs ys
  | (length xs) /= (length ys) = Nothing
  | otherwise = Just (length (differences xs ys))
