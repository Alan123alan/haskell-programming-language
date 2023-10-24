import qualified Data.Map as Map
import Data.Map (Map)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

isNucleotide :: String -> Bool
isNucleotide dnaSequence = all (\nucleotide -> elem nucleotide ['A','C','G','T']) dnaSequence

nucleotideCount :: Char -> String -> Int
nucleotideCount n dnaSequence = length (filter (\nucleotide -> nucleotide == n) dnaSequence)

nucleotidesMap :: String -> Map Nucleotide Int
nucleotidesMap dnaSequence = Map.fromList [(A, (nucleotideCount 'A' dnaSequence)),(C, (nucleotideCount 'C' dnaSequence)),(G, (nucleotideCount 'G' dnaSequence)),(T, (nucleotideCount 'T' dnaSequence))]

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
  |xs == "" = Right (Map.fromList [(A,0),(C,0),(G,0),(T,0)])
  |isNucleotide xs = Right (nucleotidesMap xs)
  |otherwise = Left "DNA Sequence contains a non nucleotide"

