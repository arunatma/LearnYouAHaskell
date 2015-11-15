-- Learn You a Haskell For Great Good
-- Chapter 13: For a Few Monads More 
-- http://learnyouahaskell.com/for-a-few-monads-more

-- Maybe and List monads covered in Ch12 - More monads in this chapter
-- All are part of "mtl" package.

import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Monoid 
import Control.Monad.Writer

{-
To see what all packages installed:

C:\> ghc-pkg list

        Cabal-1.22.4.0
        GLURaw-1.5.0.1
        GLUT-2.7.0.1
        HTTP-4000.2.20
        HUnit-1.2.5.2
        ObjectName-1.1.0.0
        OpenGL-2.12.0.1
        OpenGLRaw-2.5.1.0
        QuickCheck-2.8.1
        StateVar-1.1.0.0
        Win32-2.3.1.0
        array-0.5.1.0
        async-2.0.2
        ...
        ...
        mtl-2.2.1
        ...
        ...
-}

--------------------------------------------------------------------------------
--                          Writer  Monad                                     --    
--------------------------------------------------------------------------------
{-
    Maybe       :  For values with an added context of failure
    List        :  For values that can be non-deterministic
    IO          :  For values that interact with the real world (input / output)
    
    Writer      :  For values that has an additional log information value
-}

-- Now, onto deriving the need for a Writer monad, and the Writer monad itself!

-- Function to tell whether a cricket score is big (anything higher than 299)
isBigScore :: Int -> Bool
isBigScore x = x > 299

isBig1  = isBigScore 400                            -- True
isBig2  = isBigScore 270                            -- False

-- We also need the info, to what the score is compared against.
isBigScore' :: Int -> (Bool, String)
isBigScore' x = (x > 299, "Checked whether greater than 299")

isBig3  = isBigScore' 400       -- (True,"Checked whether greater than 299")
isBig4  = isBigScore' 270       -- (False,"Checked whether greater than 299")

{-
This works if the input is 400, 270 etc.
But, what if the input is (400, "Run Feast") or (200, "Bowling Track")

    The function we have: Takes a value and returns a value with context.
    The value (400, "Run Feast") is not just a value, but a value with context
    How to feed that to the function of type (a -> m b)
-}

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = (y, combinedLog)
    where combinedLog = log ++ newLog
          (y, newLog) = f x
      
resultWithLog1 = (400, "Run Feast.") `applyLog` isBigScore'
resultWithLog2 = (200, "Bowling Track.") `applyLog` isBigScore'

-- Let us change the function that is applied.
findLength x = (length x, "Found Length")
resultWithLog3 = ("Cricket", "Name of Sport.") `applyLog` findLength
resultWithLog4 = ("Sparrow", "Name of Bird.") `applyLog` findLength

-- Closely, looking at applyLog function
-- (++) works on any kind of list; so the type of applyLog can be 
-- applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])

-- Can 'applyLog' work on byteStrings? No, (++) function inside expects lists
-- But both lists and bytestrings are monoids and have `mappend` defined.

listEx1 = [99,104,105] `mappend` [104,117,97,104,117,97]
byteStr1 = B.pack [99,104,105] `mappend` B.pack [104,117,97,104,117,97] 

-- convert ascii to character in haskell 
charStr1 = map chr listEx1                  -- "chihuahua"  (same as byteStr1)

-- changing applyLog to work for monoid.
applyLog' :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)  
applyLog' (x, log) f = (y, combinedLog)
    where combinedLog = log `mappend` newLog
          (y, newLog) = f x

-- Now, the tuple can be (value, monoid) instead of (value, string) 

type Runs = Sum Integer
type Wicket = String

getScore :: Wicket -> (Wicket, Runs)
getScore "First" = ("Sachin", Sum (-25))
getScore "Second" = ("Sehwag", Sum (-98))
getScore "Third" = ("Dravid", Sum (-57))
getScore _ = ("Others", Sum (-20))              -- Others score 20 runs each

score1 = ("First", Sum 54) `applyLog'` getScore  -- ("Sachin",Sum {getSum = 29})
score2 = ("Second", Sum 144) `applyLog'` getScore -- ("Sehwag",Sum {getSum = 46})
score3 = ("Third", Sum 189) `applyLog'` getScore -- ("Dravid",Sum {getSum = 132})

-- First Wicket fell at 54 runs; If Sachin scored 25 runs, the rest scored "29"
-- Second Wicket fell at 144 runs; That's Sehwag who scored 98, rest scored 46 
-- Here, the monoid (Sum) gets added up, instead of (++) concatenated.

seqScore = ("Total", Sum 250) `applyLog'` getScore `applyLog'` getScore 
                                                    -- ("Others", Sum (210))
-- Out of total score of 250, if two people scoring 20 reduced, score is 210

-- logNumber :: MonadWriter w m => Int -> (Int, w) -> m Int   
logNumber x = writer (x, ["Got number: " ++ show x])  
  
-- multWithLog :: Writer [String] Int  
{-
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)  
-}    

{-------------------------------------------------------------------------------
In this chapter:
    1. Writer Monad (to be skipped, because the contents in book are out dated)
    2. Difference List
    3. Reader Monad
    4. State Monad
    5. Monadic functions
        liftM
        ap
        liftA2
        liftM2 to liftM5
        join
        filterM
    6. Safe RPN Calculator

-------------------------------------------------------------------------------}