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
import Data.List
import Data.Char
import Data.Monoid 
import Control.Monad.Writer
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import System.Random
import Control.Monad.State
import Data.Ratio
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

{-------------------------------------------------------------------------------
            Functions ((->) r)  as a monad.  (Reader Monad)
-------------------------------------------------------------------------------}

-- Functions are Functors.
f1 = (*5)
g1 = (+3)
h1 = (fmap f1 g1)       -- equivalent of h1 x = f1 (g1 x)
v1 = h1 8               -- 55       i.e. (*5)((+3) 8) = (*5) 11 = 55 

-- Functions are Applicatives
f2 = (*5)
g2 = (+3)
h2 = (+)  <$> f2 <*> g2 -- equivalent of h2 x = (f2 x) + (g2 x)
v2 = h2 8               -- 51       i.e. ((*5) 8) + ((+3) 8) = 40 + 11 = 51

{-

Monand instance for (->) r  - Located in Control.Monad.Instances

instance Monad ((->) r) where  
    return x = \_ -> x  
    h >>= f = \w -> f (h w) w  
   
* implementation of return is same as that of "pure" for applicative.
* (>>=) - Feeding a function h to another function f 
        - The result is also a function (here it is defined as the lambda fn)
        - Need to take the value out of the monad 'h' - which is done by 
          applying the function h to w (h w)
        - This result (h w) gets applied by f which is f (h w) which will be 
          a function (say ff) in this case.
        - And ff gets applied on to w.  
        - Effectively f is a function that takes 2 arguments!
-}

-- Replicating the applicative functor (h2) above.
-- But now, uses the concept of function as monad.
funcAsMonad :: Int -> Int
funcAsMonad = do
    a <- (*5)
    b <- (+3)
    return (a + b)

v3 = funcAsMonad 8              -- 51 (same as v2)

-- This function monad is called the reader monad.
-- All functions here read from a common source.  See below:

funcUsingLet :: Int -> Int 
funcUsingLet x = let
    a = (*5) x
    b = (+3) x
    in (a + b)
    
v4 = funcUsingLet 8             -- 51 (same as v2 and v3)

{-------------------------------------------------------------------------------
                State Monad.
-------------------------------------------------------------------------------}                
    
-- In Chapter 9: Input / Output, we dealt with random numbers.
-- Since Haskell is a pure functional language, for the same function call with 
-- the same input, it will give the same result always.
-- So, to get different random numbers, we needed to pass on the random number 
-- generator to each call, which is returned when 'random' function is called.
-- So, you set a seed, and for that seed, you always get the same sequence of 
-- random numbers (which is same as any other language)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = 
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in (firstCoin, secondCoin, thirdCoin)
-- (need System.Random module to be imported, for 'random' is defined there)

theToss = threeCoins (mkStdGen 33)          -- (True,False,True)

{-
    Stateful Computation:
        s -> (a, s)
    
    Take a state, return a new value, along with the new state.

    Assignment in imperative languages:
        * Can be thought of as stateful computation
        * a = 5
        * This takes the entire context (existing binding and values), 
          returns the value 'a', along with the new context (previous binding 
          and values, with a new binding of a = 5)
          
-}

{-
    Implementation of Stack using a list.
-}

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push x xs = ((), x:xs)              -- It could well be     push x xs = x:xs 
                                    -- Kept like this, coz we can covert this 
                                    -- to state type later.
                                    

-- Okay, let us take a stack, push some elements and pop a few elements 
stackOpr :: Stack -> ((), Stack)
stackOpr stack = let
    ((), stack1) = push 1 stack                 -- ((), 1:xs) 
    ((), stack2) = push 2 stack1                -- ((), 2:1:xs)
    (x, stack3) = pop stack2                    -- (2, 1:xs)
    in push 4 stack3                            -- ((), 4:1:xs)
    
newStack = stackOpr [7,8,9]                     -- ((), [4,1,7,8,9])

{-
    Ideally, we should not have worried about the previous state, and gone 
    about doing what we intended to do.
    
    stackOpr = do
        push 1
        push 2
        pop 
        push 4
        
    For this ease, presenting the State Monad.
-}

{-------------------------------------------------------------------------------
    Erstwhile definitions in Control.Monad.State
    
    newtype State s a = State { runState :: s -> (a, s) }
    
    runState is of the type, 
    runState :: State s a -> s -> (a, s)   
    
    See that runState takes a stateful computation, "State s a" and returns a 
    function of type (s -> (a, s))
    
Similar to how the definitions for "Writer" are changed to "WriterT" Monad 
    Tranformer, here 'State' newtype does not exist any more.  Now, the same 
    functionality is provided by the "StateT"

So, to be in alignment with the book example, rewriting the State definitions 
    as those existed, when the book was written.

-------------------------------------------------------------------------------}

newtype State' s a = State' { runState' :: s -> (a, s) }
-- State' s a is a 'stateful computation'
-- 's' is the State'
-- State' is a type constructor (left of "=")
-- State' is also a value constructor with type (s -> (a, s)) -> State' s a
-- Effectively, State' is a function that takes a function 'f' as input, 'f' 
-- takes a state 's' as input, and gives out a tuple (a, s) 

instance Functor (State' s) where
    fmap = liftM
 
instance Applicative (State' s) where
    pure x = State' $ \s -> (x, s)
    (<*>) = ap

instance Monad (State' s) where
    return x = State' $ \s -> (x, s)
    (State' h) >>= f = 
        State' $ \s -> let (a, newState) = h s  
                           (State' g) = f a  
                       in  g newState 

-- Explanation of (>>=)
-- Apply the stateful computation 'h' on s, which results in (a, newState)
-- Apply the function 'f' on 'a' which results in a stateful computation 'g'
-- Apply the stateful computation 'g' on the newState to get (val, finalState)                       

-- Rewriting the push and pop functions using State'

-- Here, the state 's' is the Stack 
-- stateful computation for pop is Stack' Stack Int 
pop' :: State' Stack Int  
pop' = State' $ \(x:xs) -> (x, xs)  
  
-- stateful computation for push is Stack' Stack () 
push' :: Int -> State' Stack ()  
push' a = State' $ \xs -> ((), a:xs)              

stackOpr' :: State' Stack Int  
stackOpr' = do  
    push' 1  
    push' 2  
    a <- pop'
    push' 4
    pop'
    
-- 'a' is unused in the above code snippet, which as well may be omitted.    
stackOpr'' :: State' Stack Int  
stackOpr'' = do  
    push' 1  
    push' 2  
    pop'
    push' 4
    pop'    
    
{-
    Have a look at the types of each of the functions.
    
    State' :: (s -> (a, s)) -> State' s a
        * value constructor
        * Takes a function and returns a stateful computation.
    
    runState' :: State' s a -> s -> (a, s)
        * Take a stateful computation and the current state, give out the tuple 
          containing the (value, new state)
  
    stackOpr' :: State' Stack Int
        * stackOpr' is a stateful computation of type State' Stack Int 
        
        :t runState' stackOpr'
        runState' stackOpr' :: Stack -> (Int, Stack)
        
        So, runState' stackOpr' takes a Stack i.e [Int] and returns (Int, [Int])
        
-}          

stackEx1 = runState' stackOpr' [1,2,3]          -- (4,[1,1,2,3])

-- Conditional processing.
-- Looks very similar to imperative programming!
stackCondn :: State' Stack ()  
stackCondn = do  
    a <- pop'  
    if a == 5  
        then push' 5  
        else do  
            push' 3  
            push' 8 

stackEx2 = runState' stackCondn [1,2,3]             -- ((), [8, 3, 2, 3])
stackEx3 = runState' stackCondn [5,2,3]             -- ((), [5, 2, 3])

-- Now stackOpr' and stackCondn looks very similar to push' and pop', just 
-- that these are little more complex operations inside.
-- We can combine these two, as well in a 'do' style.

moreStack :: State' Stack ()  
moreStack = do  
    a <- stackOpr'  
    if a == 4 
        then stackCondn  
        else return () 

stackEx5 = runState' moreStack [1,2,3,4,5]             -- ((), [8,3,1,2,3,4,5])

{-------------------------------------------------------------------------------
                                Get and Put
The current type signatures
get :: MonadState s m => m s
put :: MonadState s m => s -> m ()

To be in compliance with the chapter, we shall go with the book definitions 

-------------------------------------------------------------------------------}
get' :: State' s s
get' = State' $ \s -> (s, s)
-- get takes a current state and present it as the result (the first in tuple)

put' :: s -> State' s ()
put' newState = State' $ \s -> ((),newState)
-- put takes a newState and replaces the current state with the newState.

-- Now we can make the function to take (i.e get) some Stack and make a 
-- conditional execution based on the input 
checkStack :: State' Stack ()  
checkStack = do  
    stackNow <- get'  
    if stackNow == [1,2,3]  
        then put' [8,3,1]  
        else put' [9,2,1] 


chk1 = runState' checkStack [1,2,3]             -- ((), [8,3,1])
chk2 = runState' checkStack [1,2,4]             -- ((), [9,2,1])

{-

Type signatures of (>>=) bind operator.

Generic:
(>>=) :: Monad m => m a -> (a -> m b) -> m b

For State:
(>>=) :: State s a -> (a -> State s b) -> State s b  

For Maybe:
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b  

-}

{-------------------------------------------------------------------------------
            Random Numbers using State Monad.
-------------------------------------------------------------------------------}

-- type signature of 'random' function
-- random :: (RandomGen g, Random a) => g -> (a, g) 

-- See the type (g -> (a, g)) So, it is a stateful computation.
-- It can be wrapped in the State' monad.

randomSt :: (RandomGen g, Random a) => State' g a
randomSt = State' random

getThreeToss :: State' StdGen (Bool, Bool, Bool)
getThreeToss = do
    a <- randomSt
    b <- randomSt 
    c <- randomSt
    return (a, b, c)

threeToss = runState' getThreeToss (mkStdGen 33)
-- ((True,False,True),680029187 2103410263)

threeTossOnly = fst $ runState' getThreeToss (mkStdGen 33) -- (True,False,True)

{-------------------------------------------------------------------------------
                            Either Monad
-------------------------------------------------------------------------------}

-- data Either a b = Left a | Right b 

rightOnly = Right 40                -- rightOnly :: Either a Integer
leftOnly = Left "Hello"             -- leftOnly  :: Either [Char] b

-- General convention in the Either data type is 
-- Left "Error Message" | Right theValue

{-
    Monad Instance for Either (in Control.Monad.Error)
    Btw, Control.Monad.Error is deprecated - to be replaced by 
         Control.Monad.Except
    
    instance (Error e) => Monad (Either e) where 
        return x = Right x
        Right x     >>= f = f x
        Left err    >>= f = Left err
        fail msg          = Left (strMsg msg)

The instance implementation is very similar to Maybe Monad. In Maybe, if it is 
Nothing, no error information is passed, here if it is an error, Left err is 
obtained as result.        

Also, see the requirement that 'e' has to be an instance of 'Error' typeclass.

-}

eitherEx1 = Left "msg" >>= \x -> return (x + 1)         -- Left "msg"  
eitherEx2 = Right 100 >>= \x -> Left "NoWay"            -- Left "NoWay"
eitherEx3 = Right 100 >>= \x -> return (x + 1)          -- Right 101 

{-------------------------------------------------------------------------------
                            Monadic Functions
--------------------------------------------------------------------------------

liftM
Type Signature of liftM
liftM :: (Monad m) => (a -> b) -> m a -> m b 

Compare liftM with fmap 
fmap ::  (Functor f) => (a -> b) -> f a -> f b  

    Both are exactly the same, just that the wrapper is Functor in fmap, while 
    the wrapper is Monad in liftM 
    
Both fmap and liftM is defined in GHC.Base
See the source code here:
https://hackage.haskell.org/package/base-4.8.1.0/docs/src/GHC.Base.html

Implementation of liftM:

(using >>=)
liftM :: (Monad m) => (a -> b) -> m a -> m b  
liftM f m = m >>= (\x -> return (f x))  

(using 'do')
liftM   :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = do 
    x <- m
    return (f x) 
-}

liftEx1 = liftM (*3) (Just 8)                       -- Just 24
liftEx2 = fmap (*3) (Just 8)                        -- Just 24

liftEx3 = runWriter' $ liftM not $ Writer' (True, "chickpeas")  
liftEx4 = runWriter' $ fmap not $ Writer' (True, "chickpeas")  
-- (False, "chickpeas")

liftEx5 = runState' (liftM (+100) pop') [1,2,3,4] 
liftEx6 = runState' (fmap (+100) pop') [1,2,3,4] 
-- (101,[2,3,4])


-- :t runState'             :: State' s a -> s -> (a, s)
-- :t (liftM (+100) pop')   :: State' Stack Int
-- :t (fmap (+100) pop')    :: State' Stack Int

-- :t liftM (+100)          :: (Monad m, Num r) => m r -> m r
-- :t fmap (+100)           :: (Functor f, Num b) => f b -> f b

-- :t pop'                  :: State' Stack Int     (eqv of m r or f b 
--                                                   where m = f = State' Stack)

appEx1 = (+) <$> Just 3 <*> Just 5              -- Just 8
appEx2 = (+) <$> Just 3 <*> Nothing             -- Nothing

-- :t (<*>)                 :: (Applicative f) => f (a -> b) -> f a -> f b  
-- :t fmap                  :: (Functor f) => (a -> b) -> f a -> f b
-- same as fmap, but the function is also within the context, here!

{- 
    Implementing (<*>) using Monads. (ap is defined in GHC.Base)
    
    ap :: (Monad m) => m (a -> b) -> m a -> m b
    ap mf m = do
        f <- mf
        m <- x
        return (f x)
    
-}    

appEx3 = Just (+3) <*> Just 4                   -- Just 7
appEx4 = Just (+3) `ap` Just 4                  -- Just 7

appEx5 = [(+1), (+2), (+3)] <*> [10, 11]        -- [11, 12, 12, 13, 13, 14]
appEx6 = [(+1), (+2), (+3)] `ap` [10, 11]       -- [11, 12, 12, 13, 13, 14]

appEx7 = ((+) <$> Just 3) `ap` Just 5           -- Just 8      (same as appEx1)
appEx8 = ((+) <$> Just 3) `ap` Nothing          -- Nothing     (same as appEx2)

-- In Chapter 11, Applicative Functors, we say liftA2 
{-
    liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
    liftA2 f x y = f <$> x <*> y  

    In fmap or <$> the function is (a -> b), whereas in liftA2, the function is
    (a -> b -> c)
    
    liftM2 is same as liftA2, just that it has a Monad typeclass constraint.
    liftM2  :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
    liftM2 f m1 m2  = do 
        x1 <- m1
        x2 <- m2
        return (f x1 x2) 
        
    Similarly, there are liftM3, liftM4, liftM5 functions.    
-}

{-------------------------------------------------------------------------------
                            join
--------------------------------------------------------------------------------

join :: (Monad m) => m (m a) -> m a 

The way join is implemented:
join    :: (Monad m) => m (m a) -> m a
join x  =  x >>= id

In 'do' notation
join :: (Monad m) => m (m a) -> m a
join mm = do 
    m <- mm 
    m 

Implemented in GHC.Base    
-------------------------------------------------------------------------------}

joinEx1 = join (Just (Just 9))                  -- Just 9
joinEx2 = join [[1,2,3]]                        -- [1,2,3]
joinEx3 = join (Just Nothing)                   -- Nothing
joinEx4 = join Nothing                          -- Nothing
joinEx5 = join []                               -- []
-- But, join [1,2,3] will create error.

joinEx6 = join [[1,2,3],[4,5,6]]                -- [1,2,3,4,5,6]
joinEx7 = runWriter' $ join (Writer' (Writer' (1, "aaa"), "bbb"))
-- (1, "bbbaaa")

joinEx8 = join (Right (Right 9))                -- Right 9
joinEx9 = join (Right (Left "Error"))           -- Left "Error"
joinEx10 = join (Left "error")                  -- Left "error"

joinEx11 = runState' (join (State' $ \s -> (push' 10,1:2:s))) [0,0,0]
-- ((),[10,1,2,0,0,0])

-- join (State' $ \s->(push' 10,1:2:s)) :: State' Stack ()
-- (State' $ \s -> (push' 10,1:2:s))    :: Num a => State' [a] (State' Stack ())
--      or, simply
-- (State' $ \s -> (push' 10,1:2:s))    :: State' Stack (State' Stack ())

-- (\s -> (push' 10, 1:2:s))            :: Num a => [a]->(State' Stack (), [a])
-- (\s -> (push' 10, 1:2:s))            :: Stack -> (State' Stack (), Stack)

-- (push' 10, 1:2:[3,4])                :: Num a => (State' Stack (), [a])
-- (push' 10, 1:2:[3,4])                :: (State' Stack (), Stack)

{-
    See that the following two have the same type signature :: State' Stack ()
    
        push' 10
    
    and 
    
        join (State' $ \s->(push' 10, s))
    
    And, both of these effectively do the same thing!
    
-}

pushEx1 = runState' (push' 10) (1:2:[3..6])
joinEx12 = runState' (join (State' $ \s -> (push' 10,1:2:s))) [3..6]
-- Both pushEx and joinEx12 will have same output ((),[10,1,2,3,4,5,6])

{-------------------------------------------------------------------------------
        Relation between 'bind' (>>=) and 'join'
--------------------------------------------------------------------------------

    m (>>=) f       is equivalent to
        
    join (fmap f m)

    So, feeding a monadic value 'm' to a function 'f' using (>>=) is eqv. to 
    mapping the function f on the monadic value and calling join on it to take 
    out the outer context.
    
    So, instead of extacting the value from monadic value, and applying f on it,
    apply f on the monadic value and take out the outer context using 'join'
    
:t (>>=)                            :: Monad m => m a -> (a -> m b) -> m b
:t \m -> (\f -> join (fmap f m))    :: Monad m => m a -> (a -> m b) -> m b    

From the book:
"The fact that m >>= f always equals join (fmap f m) is very useful 
if we're making our own Monad instance for some type because it's often 
easier to figure out how we would flatten a nested monadic value than 
figuring out how to implement >>="

-------------------------------------------------------------------------------}

joinEx13 = join (fmap (\x -> Just (x + 1)) (Just 9))
bindEx1  = Just 9 >>= (\x -> Just (x + 1))
-- both these result in Just 10

{-------------------------------------------------------------------------------
                            filterM
--------------------------------------------------------------------------------
-- the standard filter function
filter :: (a -> Bool) -> [a] -> [a]

Instead of Bool, what if the function returns a monadic Bool value
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a] 

-------------------------------------------------------------------------------}

filterEx1 = filter (\x -> x < 4) [9,1,5,2,10,3]  
-- [1,2,3]

-- Let us provide a log of what happened here, using a Writer' monad.
keepSmall :: Int -> Writer' [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell' ["Less than 4. So, keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell' [show x ++ " is not less than 4, throwing it away"]  
        return False  
        
-- Here, keepSmall is a function that returns a "m Bool"

filterMEx1 = runWriter' $ filterM keepSmall [9,1,5,2,10,3]
{-
([1,2,3],
    ["9 is not less than 4, throwing it away",
     "Less than 4. So, keeping 1",
     "5 is not less than 4, throwing it away",
     "Less than 4. So, keeping 2",
     "10 is not less than 4, throwing it away",
     "Less than 4. So, keeping 3"])
-}

filterMEx1Logs = mapM_ putStrLn $ snd filterMEx1

-- Getting a power set for the given number using filterM 
{-
        [1,2,3]  
        [1,2]  
        [1,3]  
        [1]  
        [2,3]  
        [2]  
        [3]  
        [] 
        
    Another way to think about the PowerSet:    
    Combinations of keeping and throwing all the elements from a given list!
    
    So, instead of returning a deterministic Bool, return a non-deterministic
    Bool in the predicate function. (i.e. [Bool])
-}

powerset :: [a] -> [[a]]  
powerset xs = filterM (\x -> [True, False]) xs
-- Now, instead of getting a single list back, we would get a non-deterministic 
-- list back. ie. List of lists.

pwr3 = powerset [1,2,3] -- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

{-------------------------------------------------------------------------------
                            foldM
--------------------------------------------------------------------------------
-- the standard foldl function
foldl :: (a -> b -> a) -> a -> [b] -> a

foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a

-------------------------------------------------------------------------------}

foldEx1 = foldl (\acc x -> acc + x) 0 [2,8,3,1]         -- 14
-- (((0 + 2) + 8) + 3) + 1

-- Added Condition:
-- The whole summation has to fail, even if there is a negative number 

binPositives :: Int -> Int -> Maybe Int  
binPositives acc x  
    | x < 0     = Nothing  
    | otherwise = Just (acc + x) 

foldMEx1 = foldM binPositives 0 [2,8,3,1]       -- Just 14 
foldMEx2 = foldM binPositives 0 [2,8,3,-1]      -- Nothing

addWithLogs :: Int -> Int -> Writer' [String] Int  
addWithLogs acc x  = do  
        tell' ["Now adding " ++ show x ++ " to accumulator " ++ show acc]  
        return (acc + x)  

foldMEx3 = runWriter' $ foldM addWithLogs 0 [2,8,3,1]
{-
    (14,
        ["Now adding 2 to accumulator 0",
         "Now adding 8 to accumulator 2",
         "Now adding 3 to accumulator 10",
         "Now adding 1 to accumulator 13"]
    )
-}

{-------------------------------------------------------------------------------
                            Safe RPN Calculator
Refer to Chapter 10 for the base version of RPN Calculator implementation.
-------------------------------------------------------------------------------}

solveRPN :: String -> Double  
solveRPN = head . foldl foldingFunction [] . words  

-- foldingFunction failed when the input does not make sense 
-- For example if given something like "2 3 + +"
foldingFunction :: [Double] -> String -> [Double]  
foldingFunction (x:y:ys) "*" = (x * y):ys  
foldingFunction (x:y:ys) "+" = (x + y):ys  
foldingFunction (x:y:ys) "-" = (y - x):ys  
foldingFunction xs numberString = read numberString:xs

-- readMaybe uses "reads" function which returns a single element list 
-- on successful read, or empty list if it is a failure.
-- readMaybe converts the failure case from the context of "List" to "Maybe"
readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x  
                                _ -> Nothing  
                                
-- Make the folding function to work gracefully
safeFoldFn :: [Double] -> String -> Maybe [Double]
safeFoldFn (x:y:ys) "*" = return ((x * y):ys)       -- wrapped in the context!
safeFoldFn (x:y:ys) "+" = return ((x + y):ys)
safeFoldFn (x:y:ys) "-" = return ((y - x):ys) 
safeFoldFn xs numberString = liftM (:xs) (readMaybe numberString)

safeEx1 = safeFoldFn [3, 2] "*"         -- Just [6.0]  
safeEx2 = safeFoldFn [3, 2] "-"         -- Just [-1.0]  
safeEx3 = safeFoldFn [] "*"             -- Nothing  
safeEx4 = safeFoldFn [] "1"             -- Just [1.0]  
safeEx5 = safeFoldFn [] "1 someRandom"  -- Nothing  

-- foldM : foldes into a single value - list containing a single value 
safeSolveRPN :: String -> Maybe Double  
safeSolveRPN st = do  
    [result] <- foldM safeFoldFn [] (words st)  
    return result       -- put in Maybe Context 
    
safeEx6 = safeSolveRPN "1 2 * 4 +"                  -- Just 6.0  
safeEx7 = safeSolveRPN "1 2 * 4 + 5 *"              -- Just 30.0  
safeEx8 = safeSolveRPN "1 2 * 4"                    -- Nothing  
safeEx9 = safeSolveRPN "1 8 wharglbllargh"          -- Nothing      

{-------------------------------------------------------------------------------
                            Composing Monadic Functions
-------------------------------------------------------------------------------}

-- type signature of function compositon (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c
compF = (+3) . (*2)
compEx1 = compF 6           -- 15

-- (<=<) is monadic function composition
-- (<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c

-- They type signature can be specific as below.
-- monadCompF :: Int -> Maybe Int
-- Or, can be written in a general way as well.
monadCompF :: (Monad m, Num c) => c -> m c
monadCompF = (\x -> return (x + 3)) <=< (\x -> return (x * 2))

compEx2 = monadCompF 6 :: (((->) Int) Int)  -- compEx2 is a function takes any 
                                            -- int and gives out '15'
compEx3 = monadCompF 6 :: Maybe Int         -- Just 15 
compEx4 = monadCompF 6 :: [Int]             -- [15]

compEx5 = Just 5 >>= monadCompF             -- Just 13

-- Accumulating series of functions, using foldr
seriesFn :: Integer -> Integer
seriesFn = foldr (.) id [(*5),(*100),(+3)]   -- Note the use of foldr 

{-------------------------------------------------------------------------------
                            Knight Movement Example
-------------------------------------------------------------------------------}
-- Bringing the Knight in 3 steps example here (from Chapter 12: Monads)

type KnightPos = (Int, Int)                         -- Column, Row 

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
                ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
                ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

in3 :: KnightPos -> [KnightPos]  
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight  

canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start

-- see that in the function 'in3', we are repeatedly calling moveKnight.
-- Let us make the in3 generic, as 'inN' where we can specify N steps and 
-- get all the positions that can be reached in N steps.

-- 'replicate' needs Data.List to be imported.
-- replicate :: Int -> a -> [a]
inN :: Int -> KnightPos -> [KnightPos]  
inN x start = return start >>= foldr (<=<) return (replicate x moveKnight)

-- canReachIn will also become generic 
canReachIn :: Int -> KnightPos -> KnightPos -> Bool  
canReachIn x start end = end `elem` inN x start  

{-------------------------------------------------------------------------------
                    Creating a Monad (Probabilty Monad)
-------------------------------------------------------------------------------}
{-
    (%) is defined in Data. Ratio
    (%) :: Integral a => a -> a -> Ratio a
-}
    
-- We want a list in which each element has to have an associated probability
-- Probabilities of all elements in a list should add to 1
newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show  

-- Prob is the type constructor.  Its value constructor (also Prob) takes a 
-- list of tuples with the element and associated probability.

-- creating a functor instance.
instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs
-- apply the function only to the element and not to the associated probability.

probMult2 = fmap (*2) (Prob [(3,1%2),(5,1%4),(9,1%4)])      
-- Prob {getProb = [(6, 1 % 2),(10, 1 % 4),(18, 1 % 4)]}

probNegate = fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])  
-- Prob {getProb = [(-3, 1 % 2),(-5, 1 % 4),(-9, 1 % 4)]}


{-
    Considering a little tricky situation
        Two possible events
            x       with 25% probability 
                x can internally have two outcomes:
                a   with 50% probability
                b   with 50% probability 
                
            y       with 75% probability
                y can internally have two outcomes:
                c   with 50% probabilty
                d   with 50% probability
                
    So, overall
        a and b have 12.5% probability
        c and d have 37.5% probability 
-}

trickySituation :: Prob (Prob Char)  
trickySituation = Prob  
    [( Prob [('a', 1%2),('b', 1%2)] , 1%4 )  
    ,( Prob [('c', 1%2),('d', 1%2)] , 3%4)  
    ] 

-- Now, we need to take care of applying the outer probability to inner events.
-- Take each outer probability 'p' and multiply with inner probabilities 'r'
-- as referred below.
flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concat $ map multAll xs  
    where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p * r)) innerxs
    
-- Now we have a single list of tuples - elements with associated probabilities    
plainSituation = flatten trickySituation
-- Prob {getProb = [('a',1 % 8),('b',1 % 8),('c',3 % 8),('d',3 % 8)]}

-- We need to have an Applicative instance as well.
instance Applicative Prob where
    pure x = Prob [(x, 1%1)]
    (<*>) = ap 
    
-- Now we can create a Monad instance for 'Prob' data type.
-- If there is single element, its probability should be 1  (for 'return')
instance Monad Prob where  
    return x = Prob [(x, 1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob [] 

{-
    Monad Laws to be followed.
    
    1. return x >>= f  should be equal to f x 
    2. m >>= return    should be equal to m
    3. f <=< (g <=< h) should be equal to (f <=< g) <=< h
        -- here multiplication is associative, so this holds as well.
        
-}

-- We have a 'Probability' monad. It helps in doing calculation with 
-- joint probability, conditional probability etc.

data Coin = Heads | Tails deriving (Show, Eq)  
  
coin :: Prob Coin  
coin = Prob [(Heads, 1%2),(Tails, 1%2)]  
  
biasedCoin :: Prob Coin  
biasedCoin = Prob [(Heads, 1%10),(Tails, 9%10)]  

-- Tossing the coin three times, select only those cases where all are Tails.
flipThree :: Prob Bool  
flipThree = do  
    a <- coin  
    b <- coin  
    c <- biasedCoin  
    return (all (== Tails) [a, b, c])
    
{- 
flipThree is as follows:
   
Prob {getProb = [(False, 1 % 40), 
                 (False, 9 % 40),
                 (False, 1 % 40),
                 (False, 9 % 40),
                 (False, 1 % 40),
                 (False, 9 % 40),
                 (False, 1 % 40),
                 (True,  9 % 40)]}    
                 
When one of the coins is biased, all tails probability is 9/40!
-}

flipThree' :: Prob Bool  
flipThree' = do  
    a <- biasedCoin  
    b <- biasedCoin  
    c <- biasedCoin  
    return (all (== Tails) [a, b, c])

{- 
flipThree is as follows:
   
Prob {getProb = [(False,  1 % 1000),
                 (False,  9 % 1000),
                 (False,  9 % 1000),
                 (False, 81 % 1000),
                 (False,  9 % 1000),
                 (False, 81 % 1000),
                 (False, 81 % 1000),
                 (True, 729 % 1000)]}
                 
When all the coins are biased, all tails probability is 729/1000!                  
-}
    