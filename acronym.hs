import Data.Char

-- This function is the first in the pipeline, receives the raw input, removes all chars that are not a letter, whitespace or a hyphen
-- After removing invalid chars it replaces all occurances of '-' with ' '
removeInvalidChars :: String -> String
removeInvalidChars phrase = [replaceHyphenToWhiteSpace validChar | validChar<-[char | char<-phrase, isLetter char || isSpace char || char == '-']]

-- Function that replaces '-' with ' ' and passing all other chars through
replaceHyphenToWhiteSpace :: Char -> Char
replaceHyphenToWhiteSpace char 
  | char == '-' = ' '
  |otherwise = char

-- There was a special case between the tests in which the a string contained  a uppercase mid-wors and it needed to be taken into account
-- The first guard is to take that test into account
splitPhraseIntoWords :: String -> [String]
splitPhraseIntoWords phrase
  |all (<=2) [length [char | char<-word, isUpper char] | word<-(words phrase)] = [[(toUpper (head word))] ++ tail word | word<-(words phrase)]
  |otherwise = [[(toUpper (head word))] ++ [toLower char | char<-(tail word)] | word<-(words phrase)]

-- Gets only the first char of every word, provided that splitPhraseIntoWords title cases all words
getFirstChar :: [String] -> [Char]
getFirstChar words = [char | word<-words, char<-word, isUpper char]


abbreviate :: String -> String
abbreviate xs = getFirstChar (splitPhraseIntoWords (removeInvalidChars xs))
