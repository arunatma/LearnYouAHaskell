-- Learn You a Haskell For Great Good
-- Chapter 13: For a Few Monads More 
-- http://learnyouahaskell.com/for-a-few-monads-more

-- Maybe and List monads covered in Ch12 - More monads in this chapter
-- All are part of "mtl" package.

{- 
    The following pragma help derive automatic functor classes
    {-# LANGUAGE DeriveFunctor #-}
-}

import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Monoid 
import Control.Monad.Writer
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

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



{-------------------------------------------------------------------------------
In this chapter:
    1. Writer Monad  (new functions implemented to make book examples work)
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

-- The writer in this chapter is defined in old-fashion way. For the sake of
-- compliance with the chapter, we shall use the same.
-- Since we have Writer and runWriter defined in-built in a different way, we 
-- shall use Writer' and runWriter'

newtype Writer' w a = Writer' { runWriter' :: (a, w) } 
 
-- The following Functor and Applicative instances are taken from 
-- https://wiki.haskell.org/Functor-Applicative-Monad_Proposal  
-- In order to write a Monad instance, one has to already written the Functor 
-- and Applicative instances for the same.

instance (Monoid w) => Functor (Writer' w) where
    fmap = liftM
 
instance (Monoid w) => Applicative (Writer' w) where
    pure x = Writer' (x, mempty)
    (<*>) = ap
    
instance (Monoid w) => Monad (Writer' w) where 
    return x = Writer' (x, mempty)
    (Writer' (x, v)) >>= f = let 
        (Writer' (y, v')) = f x
        in Writer' (y, v `mappend` v')
    
-- the same instance above could also be defined as follows:
-- as defined in http://dev.stephendiehl.com/hask/
newtype Writer'' w a = Writer'' { runWriter'' :: (a, w) } 

instance (Monoid w) => Functor (Writer'' w) where
    fmap = liftM
 
instance (Monoid w) => Applicative (Writer'' w) where
    pure x = Writer'' (x, mempty)
    (<*>) = ap
    
instance (Monoid w) => Monad (Writer'' w) where
    return a = Writer'' (a, mempty)
    m >>= f = (Writer'' (y, v `mappend` v')) where 
        (x, v) = runWriter'' m
        (y, v') = runWriter'' (f x)
      
{- Inspecting the instance for Writer':

    >>= is the same as applyLog, instead of presenting the result as tuple, 
           the result is now presented in the wrapped Writer' newtype
    return is to put the value in minimal context. Here that is (a, mempty)
    
-}

writerEx1 = runWriter' (return 3 :: Writer' String Int)  -- (3, "")
writerEx2 = runWriter' (return 5 :: Writer' [Int] Int)   -- (3, [])
writerEx3 = runWriter' (return 'a' :: Writer' Any Char) 
    -- ('a', Any {getAny = False})
writerEx4 = runWriter' (return 7 :: Writer' (Sum Int) Int)
    -- (7, Sum {getSum = 0})
writerEx5 = runWriter' (return 9 :: Writer' (Product Int) Int)
    -- (9, Product {getProduct = 1})

    
logNumber :: Int -> Writer' [String] Int
logNumber x = Writer' (x, ["Got number: " ++ show x])  
  
multWithLog :: Writer' [String] Int
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)  
    
logAppendResult = runWriter' multWithLog 
-- (15,["Got number: 3","Got number: 5"])

{-
    tell is of the type signature: (in Control.MonadWriter)
    tell :: MonadWriter w m => w -> m () 
-}

-- Implementing tell function, to match the book version.
tell' :: Monoid w => w -> Writer' w () 
tell' w = Writer' ((), w)

multWithLog1 :: Writer' [String] Int  
multWithLog1 = do  
    a <- logNumber 3  
    b <- logNumber 5  
    tell' ["Gonna multiply these two"]  
    return (a*b)  
    
logAppendResult1 = runWriter' multWithLog1 
-- (15,["Got number: 3","Got number: 5","Gonna multiply these two"])


{-------------------------------------------------------------------------------
    GCD of x and y: The biggest number that divides both x and y.
    Euclid's Algorithm to find GCD of two numbers:
        1. Divide bigger number (x) by smaller number (y) - take reminder (r)
        2. Now divide y by r - take reminder.
        3. Keep doing till you get one of the numbers as 0, the other is GCD.
-------------------------------------------------------------------------------}
gcd' :: Int -> Int -> Int
gcd' a b 
    | b == 0    = a
    | otherwise = gcd' b (a `mod` b)
        
-- Incorporating log to gcd' - to see the steps it is taking.
gcd'' :: Int -> Int -> Writer' [String] Int  
gcd'' a b  
    | b == 0 = do  
        tell' ["Finished with " ++ show a]      -- Stitch Final Log
        return a                                -- Final Answer
    | otherwise = do  
        -- The incremental log (gets added as a string inside list)
        tell' [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd'' b (a `mod` b)                     -- this returns a Writer' monad
        
-- (Example from wikipedia page)
-- https://en.wikipedia.org/wiki/Euclidean_algorithm        
gcdEx1 = runWriter' $ gcd'' 1071 462     
{- 
    (21, ["1071 mod 462  = 147",
         "462 mod 147   = 21",
         "147 mod 21    = 0",
         "Finished with 21"
        ]
    )
-}

-- get only the result.
valGcdEx1 = fst gcdEx1

-- get only the logs (on to the standard output)
logGcdEx1 = mapM_ putStrLn (snd gcdEx1)

{-------------------------------------------------------------------------------
        Fast and Slow appending in the lists
-------------------------------------------------------------------------------}

{- 
Fast and Slow Lists.
In gcd'' function, the logging is fast because the list appending looks like:
    a ++ (b ++ (c ++ (d ++ (e ++ f))))  

Slow Append:
    ((((a ++ b) ++ c) ++ d) ++ e) ++ f  
    
-}

gcdReverse :: Int -> Int -> Writer' [String] Int  
gcdReverse a b  
    | b == 0 = do  
        tell' ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        result <- gcdReverse b (a `mod` b)  
        tell' [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        return result     
    
-- gcdReverse is slow because it constructs lists in the following fashion.
--  ((((a ++ b) ++ c) ++ d) ++ e) ++ f  

gcdEx2 = runWriter' $ gcdReverse 1071 462     
{- 
    (21, ["Finished with 21",
          "147 mod 21   = 0",
          "462 mod 147  = 21",
          "1071 mod 462 = 147"
         ]
    )
-}

{-------------------------------------------------------------------------------
        Difference Lists.
        
        Need:  Lists can be slow if appended in a certain way.
               We need a data structure that can efficiently take care of 
               appending!
               
        Difference List
            Equivalent DL of [1, 2, 3] is \xs -> [1, 2, 3] ++ xs 
            Equivalent DL of []        is \xs -> [] ++ xs 
            
            So, different list is actually a function of type [a] -> [a]
            
        Efficient appeding:
            f `append` g = \xs -> f (g xs)
            
-------------------------------------------------------------------------------}

f = ("first " ++)
g = ("second " ++)
-- the following line is commented as we have not implemented `mappend` yet.
-- h = f `mappend` g    -- equivalent of \xs -> "first " ++ ("second " ++ xs)

-- To make an instance of Monoid or Monad, we need to define xs++ as a newtype.
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []            -- (xs ++ ) [] which is [xs]

-- Monoid instance for DiffList.
instance Monoid (DiffList a) where 
    mempty = DiffList (\xs -> [] ++ xs)     -- add the given xs to empty list
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))
    
-- this will do [1..4] ++ ([5..10] ++ [11..20])
diffEx1 = fromDiffList (toDiffList [1..4] `mappend` toDiffList [5..10]
            `mappend` toDiffList [11..20])

-- Rewriting gcdReverse using the DiffList - to improve efficiency            
gcdReverse' :: Int -> Int -> Writer' (DiffList String) Int  
gcdReverse' a b  
    | b == 0 = do  
        tell' (toDiffList ["Finished with " ++ show a])  
        return a  
    | otherwise = do  
        result <- gcdReverse' b (a `mod` b)  
        tell' (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])  
        return result              

valGcdEx2 = fst $ runWriter' $ gcdReverse' 1000 300        
logGcdEx2 = fromDiffList $ snd $ runWriter' $ gcdReverse' 1000 300     

-- Comparing the performances!

cnt = 500000 

finalCountDown :: Int -> Writer' (DiffList String) ()  
finalCountDown 0 = do  
    tell' (toDiffList ["0"])  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell' (toDiffList [show x])

fastPrint = mapM_ putStrLn . fromDiffList. snd . runWriter' $ finalCountDown cnt
    
finalCountDown' :: Int -> Writer' [String] ()  
finalCountDown' 0 = do  
    tell' ["0"]  
finalCountDown' x = do  
    finalCountDown' (x-1)  
    tell' [show x]
    
slowPrint = mapM_ putStrLn . snd . runWriter' $ finalCountDown' cnt      

