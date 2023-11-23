import Data.Char
import Data.List

getLowerCase :: String -> String
getLowerCase word = [toLower ch|ch<-word]

isAnagram :: String -> String -> Bool
isAnagram subject candidate 
  |length subject /= length candidate = False
  |(getLowerCase subject) == (getLowerCase candidate) = False
  | (getLowerCase subject) /= (getLowerCase candidate) = length [ch|ch<-candidate, elem (toLower ch) (getLowerCase subject)] == length subject && sort (getLowerCase candidate) == sort (getLowerCase subject)

anagramsFor :: String -> [String] -> [String]
anagramsFor subject candidates = [candidate |candidate<-candidates, (isAnagram subject candidate)]
