-- romanToArabic.hs
-- Converts Roman Numerals to Decimal Number

-- MCMLXVII 1967
-- MCMXCII  1992

import Data.Map as DM

romanStr = "IVXLCDM"

values = DM.fromList
        [ ("I",  1     )
        , ("V",  5     )
        , ("X",  10    )
        , ("L",  50    )
        , ("C",  100   )
        , ("D",  500   )
        , ("M",  1000  ) ]

checkRom :: Char -> Bool
checkRom x = x `elem` romanStr
        
romanToArabic :: String -> Maybe Int
romanToArabic ""        = Just 0
romanToArabic [x]       = DM.lookup [x] values
romanToArabic (x:y:xs)  = 
    if a < b
        then (+) <$> (romanToArabic xs) <*> ((-) <$> b <*> a)
        else (+) <$> (romanToArabic (y:xs)) <*> a 
    where a = DM.lookup [x] values
          b = DM.lookup [y] values
