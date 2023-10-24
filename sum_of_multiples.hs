multiplesWithinLimit :: Integer -> Integer -> [Integer]
multiplesWithinLimit base limit
  |base == 0 = [0]
  |otherwise = takeWhile (\multiple -> multiple < limit) [base * power | power<-[1..]]


combineMultiplesWithinLimit :: [[Integer]] -> [Integer]
combineMultiplesWithinLimit listOfListsOfMultiples = [multiples | listOfMultiples<-listOfListsOfMultiples,multiples<-listOfMultiples]


deduplicateMultiplesWithinLimit :: [Integer] -> [Integer]
deduplicateMultiplesWithinLimit [] = []
deduplicateMultiplesWithinLimit (multiple:listOfMultiples) = multiple : deduplicateMultiplesWithinLimit (filter (/= multiple) listOfMultiples)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum (deduplicateMultiplesWithinLimit (combineMultiplesWithinLimit [multiplesWithinLimit base limit | base<-factors]))
