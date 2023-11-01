data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

factors :: Int -> [Int]
factors number = [divisor | divisor<-[1..(number-1)], mod number divisor == 0]

classify :: Int -> Maybe Classification
classify number
  | number <= 0 = Nothing
  | sum (factors number) == number = Just Perfect
  | sum (factors number) < number = Just Deficient
  | sum (factors number) > number = Just Abundant
