-- Learn You a Haskell For Great Good
-- Chapter 11: Functors, Applicative Functors and Monoids
-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids

import Data.Char
import Data.List  
import Control.Monad.Instances
  
-- Functors
-- Functor is a typeclass
-- Functors are things that can be mapped over (Ex: Lists, Maybe, Trees etc)
-- Functor has just a single typeclass method
-- fmap  fmap :: (a -> b) -> f a -> f b

-- Explation of fmap
-- Input: 
-- 1. A function that takes 'a' and gives out 'b'
-- 2. A box containing 'a'  (Assume 'f' is the box encompassing 'a')
-- Remember 'f' is not a function - just a box (like Maybe, List etc)
-- Output:
-- 1. A box containing 'b'  (Note that 'box' means context, not literally box)

-- Try to get the kind of the functor
-- :k Maybe or :k [] on the ghci consol
-- * -> * (means, it takes one concret type as a type parameter)

-- Any abstract type taking just one type parameter (i.e a type constructor 
-- taking a single type parameter) can be made an instance of Functor type class

-- A type can be made an instance of Functor if it takes only one type parameter
-- Maybe or []
-- "instance Functor Maybe where" is how typeclass instance defined for Maybe
-- Either takes two : Either a b. So, one parameter has to be applied
-- "instance Functor Either where"  is wrong
-- "instance Functor (Either a) where" is right
-- for this fmap might have the following declaration 
-- fmap :: (b -> c) -> Either a b -> Either a c

-- Functor instance for IO
-- already defined in GHC.Base
{- COMMENTED, as already defined in Base
instance Functor IO where  
    fmap f action = do  
        result <- action  
        return (f result)
-}
-- IO is of kind * -> * (check with :k IO in ghci)
-- Using '<-', action is bound to result (i.e momentarily taking out of the box)
-- return puts back (function applied over result) into the box and gives back

-- Objective here:
-- Mapping the function 'f' over an IO action
-- To get an IO action where the function 'f' is applied over the result

-- Following piece of code without IO Functor
-- Get a line from console, reverse it, and present it back to standard IO
getPutNoFunctor = do 
    line <- getLine   
    let line' = reverse line  
    putStrLn $ "You said " ++ line' ++ " backwards!"  
    
-- Same action, with IO Functor
getPutWithFunctor = do 
    line <- fmap reverse getLine   
    putStrLn $ "You said " ++ line ++ " backwards!"  

-- The point is take value out of IO action and apply a function over it as in 
-- the first case, or fmap the function on the IO action and get the transformed
-- value.
    
    
-- Try out these interesting pieces
maybeExample = fmap reverse (Just "This will be reversed")
listExample =  fmap (+1) [1,2,3,4,5]

prfxGetLine = fmap ("!" ++) getLine 
sufxGetLine = fmap (++ "!") getLine


-- For any case where an IO action is to be bound to a name and later a function
-- to be applied and put back in IO context and sent back, 'fmap'ping the 
-- function over IO action, makes code concise and elegant

intRevUp = do 
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
    putStrLn line 

-- the function used for fmapping above, is same as the below
-- (\xs -> intersperse '-' (reverse (map toUpper xs)))
intRevUpStr :: [Char] -> [Char]
intRevUpStr xs = intersperse '-' (reverse (map toUpper xs))
        
-- The following piece of code is not related here. A random piece of testing.
-- No harm in having here
-- Take a list and make pairs as sub lists in a list
bigram :: [a] -> [[a]]
bigram []   = []
bigram [_]  = []
bigram rest = take 2 rest : bigram (tail rest)

-- Function Functor
-- The functor: (->) r
-- Function is of the type r -> a; equivalent of (->) r a
-- In that, (->) r is an instance of  Functor (taking in the type parameter 'a')
-- (->) is similar to Either, will not be a standalone functor. It has to 
-- take a single type parameter to become a Functor (which takes another param)

-- Instance Definition for Function Functor
{- COMMENTING this piece, as it is already defined in base

instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))

-}

    
-- A little play around with the derivations
-- fmap type signature
-- fmap :: (a -> b) -> f a -> f b
-- In the above type signature 'f' refers to the Functor Context (not function)
-- Here, our functor context is (-> r)
-- Let us replace 'f' by (-> r)
-- fmap :: (a -> b) -> (-> r) a -> (-> r) b
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)         Just Rewriting
-- This is nothing, but function composition.
-- fmap :: f1 -> f2 -> f3 (takes two functions, and returns a third function)
-- f1 : Takes in a and gives out b
-- f2 : Takes in r and gives out a
-- f3 : Composition of f1 and f2 (f1 . f2) -- Takes in 'r' and gives out 'b'
-- In order, f2 will be applied on 'r' followed by applying f1 on output of f2

-- fmapping a function on "Maybe" produces a "Maybe"
-- fmapping a function on "List" produces a "List"
-- So, as well, fmapping a function on another "funtion" produces a "function"

{- Rewriting the Functor instance for the function in simpler terms

instance Functor ((->) r) where
    fmap = (.)

-}

fnComp1 = fmap (*3) (+100) 1 
fnComp2 = (*3) `fmap` (+100) $ 1        -- Same written using infix notation
fnComp3 = (*3) . (+100) $ 1             -- Using composition operator
fnComp4 = fmap (show . (*3)) (+100) 1   -- Using both fmap and (.)

-- Playing around with the meaning of fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- Means,
-- fmap takes a function and a functor (f a), returns second functor (f b)
-- But the type signature can also be written as
-- fmap :: Functor f => (a -> b) -> (f a -> f b)
-- Now this means,
-- fmap takes a function and transforms it to another function
-- where, the first function takes in 'a' and returns 'b' whereas
-- the second function takes in a functor 'f a' and returns a functor 'f b'
-- So, fmap has lifted the function to "functor" space 
-- This is, "lifting a function using fmap"

-- Explanation for this new inference - see difference between (*2) and fmap(*2)
-- :t fmap (*2) 
-- fmap (*2) :: (Num a, Functor f) => f a -> f a
-- :t (*2)
-- (*2) :: Num a => a -> a

-- :t fmap (replicate 3)  
-- fmap (replicate 3) :: (Functor f) => f a -> f [a]  
-- :t (replicate 3)
-- (replicate 3) :: a -> [a]

fmapRepList = fmap (replicate 2) [1,2,3,4]        -- [[1,1],[2,2],[3,3],[4,4]]
fmapRepMaybe = fmap (replicate 2) (Just 4)        -- Just [4, 4]
fmapRepEither = fmap (replicate 2) (Right "win")  -- Right ["win", "win"]
fmapRepNone = fmap (replicate 2) Nothing          -- Nothing
fmapRepLeft = fmap (replicate 2) (Left "foo")     -- Left "foo"

-- FUNCTOR LAWS:
-- 1. If we map the 'id' function over the functor, the functor that we get
--    back is same as the original functor
-- id is the identity function (\x -> x)
-- id :: a -> a
-- Effectively fmap id on a functor is equivalent to applying id on the functor
-- In terms of equation, 
-- fmap id = id

fmapIdMaybe = fmap id (Just 3)
idMaybe     = id (Just 3)
fmapIdList  = fmap id [3, 4]
idList      = id [3, 4]

-- 2. Composing two functions and then mapping the resulting function over a 
--    functor should be the same as mapping one function over the functor and 
--    then mapping the other one
-- In terms of equation,
-- fmap (f . g) = fmap f . fmap g
-- For any functor F
-- fmap (f . g) F = fmap f (fmap g F)

