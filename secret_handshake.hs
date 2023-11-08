toBinary :: Int -> [Int]
toBinary n
  |n == 1 = [1]
  |mod n 2 == 0 = toBinary (div n 2) ++ [0]
  |mod n 2 == 1 = toBinary (div n 2) ++ [1]

paddBinary :: [Int] -> [Int]
paddBinary bin
  |length bin == 5 = bin
  |length bin < 5 = [0 | n<-[1..(5 - (length bin))]] ++ bin

decodePaddedBinary :: [Int] -> Int -> [String] -> [String]
decodePaddedBinary paddedBin index actions
  |index == 4 && paddedBin!!index == 1 = decodePaddedBinary paddedBin (index-1) (actions ++ ["wink"])
  |index == 3 && paddedBin!!index == 1 = decodePaddedBinary paddedBin (index-1) (actions ++ ["double blink"])
  |index == 2 && paddedBin!!index == 1 = decodePaddedBinary paddedBin (index-1) (actions ++ ["close your eyes"])
  |index == 1  && paddedBin!!index == 1 = decodePaddedBinary paddedBin (index-1) (actions ++ ["jump"])
  |index == 0  && paddedBin!!index == 1 = reverse actions
  |index == 0 = actions
  |otherwise  = decodePaddedBinary paddedBin (index-1) actions

handshake :: Int -> [String]
handshake n 
  |n == 0 = []
  |otherwise = decodePaddedBinary (paddBinary (toBinary n)) 4 []

