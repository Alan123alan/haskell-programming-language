import Data.Char

cleanup :: String -> String
cleanup phone = [digit | digit<-phone, isDigit digit]

number :: String -> Maybe String
number phone
  -- Discarding any phone number shorter than 10 digits after clean up
  | length (cleanup phone) < 10 = Nothing
  -- Discarding any phone number longer than 11 digits after clean up
  | length (cleanup phone) > 11 = Nothing
  -- Discarding any phone number of 11 digits after clean up that doesn't start with 1
  | length (cleanup phone) == 11 && head (cleanup phone) /= '1' = Nothing
  | length (cleanup phone) == 11 && (cleanup phone)!!1 == '0' = Nothing
  | length (cleanup phone) == 11 && elem ((cleanup phone)!!1) ['0', '1'] = Nothing
  | length (cleanup phone) == 11 && elem ((cleanup phone)!!4) ['0', '1'] = Nothing
  | length (cleanup phone) == 10 && elem ((cleanup phone)!!0) ['0', '1'] = Nothing
  | length (cleanup phone) == 10 && elem ((cleanup phone)!!3) ['0', '1'] = Nothing
  | head (cleanup phone) == '1' = Just (tail (cleanup phone))
  | otherwise = Just (cleanup phone)
