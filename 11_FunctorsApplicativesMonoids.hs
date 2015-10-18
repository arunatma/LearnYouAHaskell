-- Learn You a Haskell For Great Good
-- Chapter 11: Functors, Applicative Functors and Monoids
-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids

import Data.Char
import Data.List  
import Control.Applicative
  
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
-- :k Maybe or :k [] on the ghci console
-- * -> * (means, it takes one concrete type as a type parameter)

-- Any abstract type taking just one type parameter (i.e a type constructor 
-- taking a single type parameter) can be made an instance of Functor type class

-- A type can be made an instance of Functor if it takes only one type parameter
-- The likes of Maybe, [] etc.,
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
fnComp3 = (*3) . (+100) $ 1             -- Using composition operator	(303)
fnComp4 = fmap (show . (*3)) (+100) 1   -- Using both fmap and (.)		("303")

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


-- fmap (f . g) Nothing == Nothing
-- fmap f (fmap g Nothing) == Nothing
-- fmap (f . g) (Just 5) == fmap f (fmap g (Just 5))
-- fmap ((+2) . (*3)) (Just 5) == Just 17
-- fmap (+2) (fmap (*3) (Just 5)) == Just 17

-- A hypothetical data type to show that being an instance of Functory typeclass
-- does not guarantee it to be a functor
-- A functor should obey both the functor laws

data CMaybe a = CNothing | CJust Int a deriving (Show)
instance Functor CMaybe where  
    fmap f CNothing = CNothing  
    fmap f (CJust counter x) = CJust (counter+1) (f x) 

-- Testing the identity functor law
testId0 = id (CJust 0 "haha")           -- 0
testId1 = fmap id (CJust 0 "haha")      -- 1
-- id (f) == fmap id (f)  where f is a functor;  Here, this law is not satisfied

-- Just, make sure when creating the instances that the functor obeys both the
-- functor laws


-- Applicative Functors
-- Present in Control.Applicative (import the same)

-- Haskell is curried, in terms of function application 
-- And left associative
-- a -> b -> c -> d 
-- d = f a b c which is also d = ((f a) b) c

-- What if the map function done over functor with fmap, takes two parameters?
fnInContext1 = fmap (*) (Just 3) 
-- Try to get the type of fnInContext1 using :t
-- fmap (*) (Just 3) :: Num a => Maybe (a -> a)
-- So, it is a function that takes a 'Num' and gives out a 'Num', but the 
-- function is present inside the Maybe context 
-- fmap applies the function (*) to what is inside the context (here 'Just')
-- so, we effectively get Just (*3)

-- A few more examples
fnInContext2 = fmap (++) (Just "hey")   -- equivalent of Just ("hey" ++)
                                        -- Maybe ([Char] -> [Char])

fnInContext3 = fmap compare (Just 'a')  -- equiv. of Just (compare 'a')
                                        -- Maybe (Char -> Ordering)

fnInContext4 = fmap compare "A String"  -- [Char -> Ordering]

fnInContext5 = fmap (\x y z -> x + y / z) [3, 4, 5, 6] 
                                        -- (Fractional a) => [a -> a -> a]
-- In the above case, the function needs 3 inputs out of which only 'x' is 
-- supplied as 3, 4, 5, 6.  So, we get a list of functions expecting 'y' and 'z'
-- and producing 'z'

fnInContext6 = fmap (*) [1, 2, 3, 4]    -- equiv. of [(*1), (*2), (*3), (*4)]
                                        -- Num a => [a -> a]

-- Now, let us supply arguments to these functions present inside the context

mul9 = fmap (\x -> x 9) fnInContext6    -- [9, 18, 27, 36]

-- Functors: Help in mapping normal functions over the functors
-- Applicative Functors: Help in mapping function inside a functor over another 
-- functor.

-- In the Control.Applicative, the following class is defined.
{-
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b 

-}
-- Just the class is defined, no default implementation is provided. It is left
-- to the users in the instance definition.

-- Any type to be an instance of 'Applicative' should first be instance of 
-- Functor

-- Looking at the above,
-- pure takes in a type and puts it in the Applicative Functor context 'f'
-- (<*>) takes in a function within a context 'f' and a type 'a' also within the
-- same context and returns another type 'b' within the same context 
-- (<*>) is very similar to fmap whose type signature is (a -> b) -> f a -> f b 
-- Just that, here the function that (<*>) takes is put inside the context 


{-
1. Applicative Instance implementation for 'Maybe'

instance Applicative Maybe where 
    pure = Just
    Nothing <*> _           = Nothing
    (Just f) <*> something  = fmap f something 

    -- here f in (Just f) is a function 
-}

-- No function can be extracted from 'Nothing', so the result will be 'Nothing'
-- If a function can be extracted ('f'), that is fmap applied to something
-- As per the definition of 'Applicative' both the arguments of (<*>) are 
-- functors i.e f (a -> b) and f a 
-- So, in fmap f something, 'something' is a functor 

applicativeEx1 = Just (+3) <*> Just 9           -- Just 12
applicativeEx2 = pure (+3) <*> Just 10          -- Just 13
applicativeEx3 = Just (++ "hello") <*> Just "hi"    -- "hihello" 
applicativeEx4 = Just ("hello" ++) <*> Just "hi"    -- "hellohi"
applicativeEx5 = Just (++ "hello") <*> Nothing  -- Nothing 
applicativeEx6 = Nothing <*> Just 9             -- Nothing 

-- Operate on several functors with a single function
applicativeChain1 = pure (+) <*> Just 3 <*> Just 5      -- Just 8
-- (pure (+) <*> Just 3) <*> Just 5
-- Just (+3) <*> Just 5
applicativeChain2 = pure (+) <*> Just 3 <*> Nothing             -- Nothing
applicativeChain3 = Nothing <*> Just 3 <*> Just 2               -- Nothing
applicativeChain4 = Nothing <*> Just 3 <*> Just 2 <*> Just 1    -- Nothing
-- But the following will throw error
-- applicativeChain5 = pure (+) <*> Just 3 <*> Just 2 <*> Just 1    

-- The following is perfectly fine
applicativeChain6 = pure (\x y z -> x + y + z) <*> Just 3 <*> Just 2 <*> Just 1    

-- The general format is
-- pure f <*> apFunctor1 <*> apFunctor2 <*> apFunctor3 <*> ...

-- pure f <*> x is the equivalent of fmap f x 
-- So, a special function <$> is defined in Control.Applicative which does that 
{-

(<$>) :: (Functor f) => (a -> b) -> f a -> f b 
f <$> x = fmap f x 

f in the type signature refers to functor 
f in the function definition refers to a function 
-}

-- Now, with the use of <$>, the applicativeChain6 can be rewritten as 
applicativeChain7 = (\x y z -> x + y + z) <$> Just 3 <*> Just 2 <*> Just 1    
-- of the format 
--                              f <$> x <*> y <*> z
-- which, of course is same as 
--                         pure f <*> x <*> y <*> z

applicativeChain8 = (++) <$> Just "Sachin" <*> Just "Tendulkar"
normalEqvApChain8 = (++) "Sachin" "Tendulkar"

-- <$> and <*> helps to operate the normal function on applicative functors!!

-- Try the following in ghci
-- :t (++)                      [a] -> [a] -> [a]
-- :t (++) <$> Just "Sachin"    Maybe ([Char] -> [Char]) Function within Context
-- :t (++) <$> Just "Sachin" <*> Just "Tendulkar"   Maybe [Char] 

{-
2. Applicative instance for Lists []

instance Applicative [] where 
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
-}
-- Every function to the left of <*> is applied to every element on the right 


-- Both the arguments of <*> should be of the same type 
-- Here it is []
-- The first argument should be a function enclosed in the context 
-- i.e Maybe Function or [List of Functions] or any other applicative 

-- 'pure' takes a value and put in a minimal context
-- Like Just x, here it is [x]
-- Though 'Nothing' or [] are the minimal context, the value 'x' cannot be 
-- expressed in it.

-- pureEx1 = pure "Hi"                     -- "Hi" (works only in ghci)
-- In ghci, it takes as function application context, whereas here, it does not
-- understand, what context it is in 
pureEx2 = pure "Hi" :: Maybe String     -- Just "Hi"
pureEx3 = pure "Hi" :: [String]         -- ["Hi"]

-- The type of <*> if it were to work for only lists 
-- (<*>) :: [a -> b] -> [a] -> [b]
-- The type of <*> if it were to work for only 'Maybe' 
-- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b

listApp1 = [(*0),(+100),(^2)] <*> [1,2,3]       -- list of 9 elements 
-- [0, 0, 0, 101, 102, 103, 1, 4, 9]
listApp2 = [(flip (-) 100),((-)100),(2^)] <*> [1,2,3]
-- [-99, -98, -97, 99, 98, 97, 2, 4, 8]

listApp3 = [(+),(*)] <*> [1,2] <*> [3,4]
-- [(+)1, (+)2, (*)1, (*)2] <*> [3, 4]
-- [(+) 1 3, (+) 1 4, (+) 2 3, (+) 2 4, (*) 1 3, (*) 1 4, (*) 2 3, (*) 2 4]
-- [4, 5, 5, 6, 3, 4, 6, 8]

-- <*> is left associative, so start applying the functions from the left 

listApp4 = (++) <$> ["ha", "heh", "hmm"] <*> ["?", "!", "."]
-- [(++) "ha", (++) "heh", (++) "hmm"] <*> ["?", "!", "."]
-- [(++) "ha" "?", (++) "ha" "!", (++) "ha" "." ....]
-- ["ha?", "ha!", "ha.", "heh?", "heh!", "heh.", "hmm?", "hmm!", "hmm."]

-- Lists are for non-deterministic computation
-- (+) 3 2 will always give 5
-- But if one wants to add elements present in a list 
-- (+) <$> [2, 3] <*> [4, 6]        -- [6, 8, 7, 9] (there is no single output)

listApp5 = (*) <$> [2, 5, 10] <*> [8, 10, 11]
eqLstAp5 = [x * y | x <- [2,5,10], y <- [8,10,11]] 
-- This is nothing but the list instance implementation for Applicative

-- A little probe into (*) <$> [2, 5, 10]
-- f <$> xs 
-- pure f <*> xs 
-- [f] <*> xs 
-- The catch is that, with the use of <$>, there is just one funtion that 
-- can be applied, because <$> uses "fmap"

{-
3. Applicative Instance for IO

instance Applicative IO where
    pure = return
    a <*> b = do 
        f <- a
        x <- b
        return (f x)
-}
 
-- pure is putting the element in minimal context 
-- 'return' is the minimal in the IO context 
-- return makes an IO action, but really does not do anything 
-- puts the element in the IO context, that's it.

-- Type signature for <*> if it were specialized for IO
-- <*> :: IO (a -> b) -> IO a -> IO b

-- Means, 
-- Take an IO action, which yields a function as a result (a -> b)
-- Take another IO action, which yields a value (a) 
-- Perform another IO action with the function applied on the value, as result 

-- Effectively,
-- We are taking two IO actions (sequencing them) and make into one.
-- Perform the sequenced IO operation to produce the result 

ioWithoutAppFun :: IO String 
ioWithoutAppFun = do 
    a <- getLine
    b <- getLine
    return (a ++ b)
    
-- This performs two IO actions (two getLine), binds those with variables 'a' 
-- and 'b' and concatenate, put in the IO context (return)
-- ioWithoutAppFun is of type "IO String", which means the output will be a 
-- string in the IO context 

ioWithAppFun :: IO String
ioWithAppFun = (++) <$> getLine <*> getLine

-- This is intuitive to understand
-- (++) takes two strings as inputs (or two lists, in general)
-- In applicative functor context, the string is encompassed in IO with getLine 
-- And concatenated with another string encompassed in IO with the 2nd getLine
-- (++) <$> getLine <*> getLine

-- If you ever find yourself binding some I/O actions to names and then calling 
-- some function on them and presenting that as the result by using return, 
-- consider using the applicative style because it is more concise and terse.

{-
4. Applicative Instance for (-> r)

instance Applicative ((->) r) where 
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
    
-}

-- type of "pure" (if it were only to ((->) r)
-- pure :: a -> (r -> a)
-- puts in a minimal context (which is the value itself)

fnAppFn1 = pure 3 "blah"            -- 3 
--  fnAppFn2 = pure 4               -- is not syntactically correct 
fnAppFn3 = (pure 3) "blah"          -- 3
-- pure 3 "blah"
-- (pure 3) "blah"
-- (\_ -> 3) "blah"                 -- function that takes argument "blah"
                                    -- and ignores it, and always return 3 

-- Let us look at <*> for the ((->) r) applicative functor
-- :t (+) <$> (+3) <*> (*100)
-- Num a => a -> a 
-- Creates a function, that takes a Num and returns a Num 
fnAppFn4 = (+) <$> (+3) <*> (*100)  -- Take a num (add 3) and (multiply 100)
                                    -- then, add both 
                                    -- ((+3) x) + ((*100) x)
fnAppFn4Ex1 = fnAppFn4 3            -- 306 
fnAppFn4Ex2 = fnAppFn4 6            -- 609

fnAppFn5 = (+) <$> (+3) <*> (*100) $ 5 -- 508 (direct application)

-- Here, the inputs to <*> are two functions
-- So, we get back another function as the result 
fnAppFn6 =  (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5  -- [8, 10, 2.5]

-- Generic format:
-- k <$> f <*> g $ x
-- Call function 'k' with the results of f and g 
-- f and g themselves are function applied over x 

{-
5. Applicative Instance for ZipList (resides in Control.Applicative

instance Applicative ZipList where 
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList gs = ZipList (zipWith (\f x -> f x) fs xs)
    
-}

noZipEx1 = [(+3), (*2)] <*> [1, 2]              -- [4, 5, 2, 4]
-- what if we want the first fn to be applied to first fn, 2nd to 2nd and so on
-- the result should be [4, 4]
-- Introducing ZipList
zipEx1 = getZipList $ ZipList [(+3),(*2)] <*> ZipList [1, 2]    -- [4, 4]
zipEx2 = getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100] 

-- pure is putting something in minimal context 
-- pure in ZipList context should be an infinite list, so it can be lazily
-- taken and applied to the second argument 
zipEx3 = getZipList $ pure (*2) <*> ZipList [1,5,10]        -- [2, 10, 20]
zipEx4 = take 4 $ getZipList $ pure (*2) <*> (pure 7)       -- [14, 14, 14, 14]

-- getZipList $ pure (*2) <*> (pure 7) is an infinite list of [14, 14....]

zipEx5 = getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
zipEx6 = getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
-- [('d','c','r'),('o','a','a'),('g','t','t')]  

-- zipWith takes a function and two lists (operates the function on taking 
-- one element for each list)
-- Like zipWith, there is zipWith3, zipWith4 ... zipWith7 (taking 3, 4 ..7 
-- arguments for the function respectively)
-- With ZipList, we can use applicative functor to do even zipWith50 zipWith100,
-- though, those are practically not needed.

-- liftA2 function
-- defined in Control.Applicative
liftA2' :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c 
liftA2' f a b = f <$> a <*> b 

-- applying a normal function on two applicative functors and getting an
-- applicative functor as the result
-- Also can be looked at as
-- (a -> b -> c) -> (f a -> f b -> f c)
-- Take a normal function that operates on two values, lift it to operate on
-- two applicative functors 

-- fmap operated as lift
liftEx1 = fmap (\x -> [x]) (Just 4)         -- Just [4]
-- using liftA2, because the function takes two arguments
liftEx2 = liftA2 (:) (Just 3) (Just [4])    -- Just [3, 4]

-- generalizing liftA2 function (to take more arguments)
-- sequence function (defined in Control.Applicative)
-- Transform a list of applicatives to an applicative with a list 
sequenceX :: (Applicative f) => [f a] -> f [a]
sequenceX [] = pure []
sequenceX (x:xs) = (:) <$> x <*> sequenceX xs

liftEx3 = sequenceX [Just 3, Just 4]    -- same as liftEx2
-- same as (:) <$> (Just 3) <*> (Just [4])
-- sequenceX [Just 3, Just 4]
-- (:) <$> Just 3 <*> sequenceX [Just 4]
-- (:) <$> Just 3 <*> ( (:) <$> Just 4 <*> sequenceX [])
-- (:) <$> Just 3 <*> ( (:) <$> Just 4 <*> (pure []))
-- (:) <$> Just 3 <*> ( (:) <$> Just 4 <*> (Just []))
-- (:) <$> Just 3 <*> ( Just [4] )
-- Just [3, 4]

-- sequenceX, second implementation
-- using liftA2 and foldr
sequenceX1 :: (Applicative f) => [f a] -> f [a]
sequenceX1 = foldr (liftA2 (:)) (pure [])
-- foldr, start from the right and keep folding towards the left 
-- take each element one by one from the right 

seqEx1 = sequenceX [Just 3, Just 2, Just 1]           -- Just [3, 2, 1]
seqEx2 = sequenceX [Just 3, Nothing, Just 1]          -- Nothing

seqEx3 = sequenceX [(+3), (*2), flip (-) 1] 5         -- [8, 10, 4]
seqEx4 = sequenceX [[1,2,3],[4,5,6]]       -- [[1,4], [1,5], [1,6], [2,4],...]

-- seqEx3
-- Using sequence with a list of functions and a single value
-- Call each of the function with the value as the operator 
-- Combine all to a list 

-- See a value that satisfies a list of predicates
mapEx1 = map (\f -> f 7) [(> 4), (<10), odd]  -- map a fn on a list of fns
mapEx2 = and $ mapEx1                         -- 'and'  the entire list 

-- Same functionality using sequenceX
seqEx5 = sequenceX [(> 4), (<10), odd] 7         -- [True, True, True]
seqEx6 = and $ seqEx5                           -- True 

-- So, what is the difference between [(> 4),(< 10),odd]
-- and sequenceA [(> 4),(< 10),odd]
-- [(> 4),(< 10),odd]           :: Integral a => [a -> Bool]
-- sequenceA [(> 4),(< 10),odd] :: Integral a => a -> [Bool]

-- Fine difference - the first is a list of fns each taking 'a' and giving Bool 
-- the second is a function that takes 'a' and gives a list of Bool 
-- So, list of applicatives is transformed to applicative with a list 

-- With [], the sequenceX takes a list of lists and gives another list of lists
seqEx7 = sequenceX [[1,2,3],[4,5,6]]  
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
-- same as list comprehension
eqvSeqEx7 = [[x, y] | x <- [1, 2, 3], y <- [4, 5, 6]]

seqEx8 = sequenceX [[1,2],[3,4],[5,6]]
eqvSeqEx8 = [[x, y, z] | x <- [1, 2], y <- [3, 4], z <- [5, 6]]

-- sequenceX [[1,2],[3,4]]
-- (:) <$> [1,2] <*> sequenceX [3,4]
-- (:) <$> [1,2] <*> ((:) <$> [3,4] <*> sequenceX [])
-- (:) <$> [1,2] <*> ((:) <$> [3,4] <*> pure [])
-- (:) <$> [1,2] <*> ((:) <$> [3,4] <*> [[]])
-- (:) <$> [1,2] <*> ((:) <$> [3,4] <*> [[]])
-- (:) <$> [1,2] <*> ([3:[], 4:[]])
-- (:) <$> [1,2] <*> [[3], [4]]
-- [1:[3], 1:[4], 2:[3], 2:[4]]
-- [[1,3], [1,4], [2,3], [2,4]]

-- sequenceX can be used with IO actions as well (same as 'sequence')

seqIO1 = sequenceX [getLine, getLine, getLine]      -- ["one", "two", "three"]

{-
-- Applicative Functor Laws:

    pure f   <*> x              = fmap f x 
    pure id  <*> v              = v
    pure (.) <*> u <*> v <*> w  = u <*> (v <*> w) 
    pure f   <*> pure x         = pure (f x)
    u        <*> pure y         = pure ($ y) <*> u 

Todo:

Need to find examples for each of these laws.

-}


-- newtype
-- getting back to the example of ZipList
compreEx = [(+1),(*100),(*5)] <*> [1,2,3]  	-- gives out 9 elements as output
zipEx	 = getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]  
-- compreEx will have [2,3,4,100,200,300,5,10,15]
-- zipEx will have [2,200,15]

{-

Possibilities of ZipList definition

1. Using 'data' keyword

			data ZipList a = ZipList [a]

	ZipList to the left of '=' is the type constructor
	ZipList to the right of '=' is the value constructor
	ZipList value constructor (to the right of '=') is a function of type
	ZipList :: [a] -> ZipList a
	
2. Using 'data' keyword and record syntax
	
			data ZipList a = ZipList {getZipList :: [a]}
	
	The record syntax helps in getting the list out of ZipList
	ZipList 	:: [a] -> ZipList a
	getZipList 	:: ZipList a -> [a]

-}

-- Effectively, what happens in ZipList
-- [a] is wrapped in a new data type and presented as ZipList
-- For this very purpose, we have 'newtype'

{- 

Actual ZipList definition

			newtype ZipList a = ZipList { getZipList :: [a] }  


1. newtype is faster (actual wrapping / unwrapping not performed, as is the 
   case with 'data' keyword)
2. The compiler understands that it is the same data type wrapped into
   something else
3. newtype - just like any other data type, can derive Eq, Ord, Show, Read, 
   Enum and Bounded 
   
-}

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

chLstEx1 = CharList "Cool!"			-- this has a Show instance (derived)
chLstEx2 = getCharList chLstEx1		-- "Cool!"

chLstEx3 = CharList "Hello" == CharList "Hello"		-- True
chLstEx4 = CharList "Well" == CharList "Good"		-- False

{--

type signatures

CharList :: [Char] -> CharList  
getCharList :: CharList -> [Char]  

--}

