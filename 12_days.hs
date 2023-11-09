getDay 1 = "first"
getDay 2 = "second"
getDay 3 = "third"
getDay 4 = "fourth"
getDay 5 = "fifth"
getDay 6 = "sixth"
getDay 7 = "seventh"
getDay 8 = "eighth"
getDay 9 = "ninth"
getDay 10 = "tenth"
getDay 11 = "eleventh"
getDay 12 = "twelfth"

intro :: String -> String
intro day = "On the "++day++" day of Christmas my true love gave to me: "

getGifts :: Int -> Int -> String
getGifts dayOriginal dayRecurse
  |dayOriginal == 1 && dayRecurse == 1 = "a Partridge in a Pear Tree."
  |dayOriginal /= 1 && dayRecurse == 1 = "and a Partridge in a Pear Tree."
  |dayRecurse == 2 = "two Turtle Doves, " ++ (getGifts dayOriginal (dayRecurse-1))
  |dayRecurse == 3 = "three French Hens, " ++ (getGifts dayOriginal (dayRecurse-1))
  |dayRecurse == 4 = "four Calling Birds, " ++ (getGifts dayOriginal (dayRecurse-1))
  |dayRecurse == 5 = "five Gold Rings, " ++ (getGifts dayOriginal (dayRecurse-1))
  |dayRecurse == 6 = "six Geese-a-Laying, " ++ (getGifts dayOriginal (dayRecurse-1))
  |dayRecurse == 7 = "seven Swans-a-Swimming, " ++ (getGifts dayOriginal (dayRecurse-1))
  |dayRecurse == 8 = "eight Maids-a-Milking, " ++ (getGifts dayOriginal (dayRecurse-1))
  |dayRecurse == 9 = "nine Ladies Dancing, " ++ (getGifts dayOriginal (dayRecurse-1))
  |dayRecurse == 10 = "ten Lords-a-Leaping, " ++ (getGifts dayOriginal (dayRecurse-1))
  |dayRecurse == 11 = "eleven Pipers Piping, " ++ (getGifts dayOriginal (dayRecurse-1))
  |dayRecurse == 12 = "twelve Drummers Drumming, " ++ (getGifts dayOriginal (dayRecurse-1))

recite :: Int -> Int -> [String]
recite start stop = [(intro (getDay day)) ++ (getGifts day day) | day <-[start..stop]]

