-- Learn You a Haskell For Great Good
-- Chapter 12: A Fistful of Monads
-- http://learnyouahaskell.com/a-fistful-of-monads

{- 

A recap:

Functors:  Contextual values that can be mapped over by a function

fmap :: (Functor f) => (a -> b) -> f a -> f b
Takes a function (a -> b) and a value in a context 'f a' and gives 'f b'


Applicative Functors: Contextual values that can be mapped over by a function
                      which too is within a context

<*> :: (Applicative Functor f) => f (a -> b) -> f a -> f b 
Takes a function within contex f (a -> b) and a value in a context 'f a' and 
gives 'f b'
           
-}

appEx1 = (*) <$> Just 2 <*> Just 8                  -- Just 16
appEx2 = (++) <$> Just "klingon" <*> Nothing        -- Nothing
appEx3 = (-) <$> [3,4] <*> [1,2,3]                  -- List Comprehension

-- What is the context here?
-- Maybe is for something that could have failed or not taken a value (Nothing)
-- List is for non-deterministic computations (so, give out all possible 
-- combination of computations - List Comprehension)
-- IO is for values that may have side-effects - things affecting real world

{-

Monads: Can be considered an extension of Applicative Functors

Makes it mandatory to define the following function (called as bind)                      
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b 

    1. Bind takes a monad value (monad contextual 'a'), and a function that 
    takes a normal 'a', returning a monad contextual 'b'
    2. The return value of the bind operation is a monad contextual 'b'

All Monads are Applicative Functors, and 
All Applicative Functors are Functors 

-}
    
-- Taking the case of "Maybe"
-- Maybe as a functor
funMaybe1 = fmap (++ "!") (Just "Cricket")          -- Just "Cricket!"
funMaybe2 = fmap ('C':) (Just "hat")                -- Just "Chat"

-- Maybe as an applicative functor
appFunMaybe1 = Just (+3) <*> Just 4                 -- Just 7
appFunMaybe2 = Nothing <*> Just "Something"         -- Nothing
appFunMaybe3 = (++) <$> Just "Hi" <*> Just "Hello"  -- Just "HiHello"

{-- 
Maybe as a monad:

(>>=) to be implemented:
Input:  Take a monodic value, and a function that takes a normal value and 
        returning a monadic value 
Output: Monadic Value

In Maybe context, for (>>=):
Input:  Take a Maybe value and a function that takes a normal value and returns a Maybe 
value.
Output: Maybe value 

Type Signature:  m a -> (a -> mb) -> m b 

--}

{-
Building the Maybe Monad:
Step 1: Try to build a funtion which is of type signature
        a -> (a -> mb) -> mb 
        Instead of taking a monadic value, we are taking a normal value
-}

insideFn1 :: Int -> Maybe Int 
insideFn1 x = Just (x + 1)

insideFn2 :: String -> Maybe String
insideFn2 x = Just (x ++ "!")

monadBuildFn :: a -> (a -> Maybe a) -> Maybe a 
monadBuildFn x f = f x

monadBuildEx1 = monadBuildFn 4 insideFn1                -- Just 5
monadBuildEx2 = monadBuildFn "Hello" insideFn2          -- Just "Hello!"

{-
Building the Maybe Monad:
Step 2: Instead of 
        a -> (a -> mb) -> mb 
        
        Now, Implement the following type signature for (>>=)
        m a -> (a -> mb) -> mb
        
What's the difference?
    monadBuildFn takes a normal value 'a' 
    Bind (>>=) takes in a monadic value 'm a'        
    
-}

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
bindMaybe Nothing f  = Nothing  
bindMaybe (Just x) f = f x 

bindMaybeEx1 = bindMaybe (Just 4) insideFn1             -- Just 5
bindMaybeEx2 = bindMaybe (Just "Hello") insideFn2       -- Just "Hello!"

-- Using as infix operator 
bindMaybeEx3 = (Just 5) `bindMaybe` insideFn1                   -- Just 6
bindMaybeEx4 = (Just "Hi") `bindMaybe` \x -> Just ("Say" ++ x)  -- Just "SayHi"

insideFn3 :: Int -> Maybe Int 
insideFn3 x = if x > 5 then Just (x + 1) else Nothing 

bindMaybeEx5 = (Just 5) `bindMaybe` insideFn3           -- Nothing     

-- Notice:
-- monadBuildFn is generalized
-- bindMaybe is a particular function case for just "Maybe" monad 

{- 

Monad Typeclass

class Monad m where
    return  :: a -> m a
    
    (>>=)   :: m a -> (a -> m b) -> m b
    
    (>>)    :: m a -> m b -> m b 
    x >> y = x >>= (\_ -> y)
    
    fail    :: String -> m a
    fail msg = error msg

Any type which implements these four functions and follow the monad laws is 
said to be a monad.
    
-}

-- Though there is no typeclass constraint in the Monad class definition,
-- All Monads are Applicative Functors
-- Ideally, it should have been written as (Applicative m) => Monad m 
-- But, Monads came much ahead than Applicative Functors 

{-

1. Putting in the context

For Applicative Functors:   "pure"

For Monads:                 "return"
    return for "Maybe" is Just 
    return for IO is putting in IO context 
    

-}

{-

2. Function Application

For Applicative Functors:   <*>
    <*> :: f (a -> b) -> f a -> f b 

For Monads:                 (>>=)
    (>>=) :: m a -> (a -> m b) -> m b 
    1. The first two arguments are order-swapped in applicative functor v Monad 
    2. Applicative functor takes a function within the context
       In Monad, the function takes a normal value and returns a monadic value 
    
-}

{-

3. (>>) : This comes with a default implementation, and most likely not needed
          to change 

-}

{-

4. fail : To be explained later  

-}


-- MONAD INSTANCES

{-

1. Instance for Maybe 

instance Monad Maybe where
    return x        = Just x
    
    Nothing >>= f   = Nothing
    Just x >>= f    = f x
    
    fail _          = Nothing 

    
See that, we have not altered the default implementation of (>>). We did only 
the other three functions     

-- 'return' is same as pure of applicative functor
-- (>>=) is same as what we say as 'bindMaybe'

-}

monadMaybeEx1 = return "What" :: Maybe String           -- Just "What"
monadMaybeEx2 = Just 9 >>= \x -> return (x * 10)        -- Just "90"
monadMaybeEx3 = Nothing >>= \x -> return (x * 10)       -- Nothing 

-- 9 is taken in for the calculation (x * 10) from Just 9, without doing 
-- explicit pattern matching (it is done in the instance definition!)

-- Using the (>>=) bind function, (Maybe a) can be fed to (a -> Maybe a) fn.

{-

Maybe Monad - Explanation using an example

Fulcrum:
    Pole positioned at centre on a fulcrum
    Birds sit on pole on either sides
    If the difference in number of birds on one side to another exceeds 3, 
        the stick loses balance
    Birds keep landing and taking off from either side.
-}

type Birds  = Int                -- Type synonym
type Pole   = (Birds, Birds)     -- Pole defined by how many birds on both sides

landLeft :: Birds -> Pole -> Pole 
landLeft n (x, y) = (x + n, y)

landRight :: Birds -> Pole -> Pole
landRight n (x, y) = (x, y + n)

-- The birds land in this order, on empty pole 
-- One on left, One on right, then, two on left 
poleEx1 = landLeft 2 (landRight 1 (landLeft 1 (0,0)))           -- (3, 1)

-- Can we write this better
-- Define a operator function (-:)
x -: f = f x
-- Inferred type of above function 
-- checked using :t (-:)   
-- (-:) :: t1 -> (t1 -> t) -> t

-- Now, we can write the same in a left to right readable fashion
poleEx2 = (0, 0) -: landLeft 1 -: landRight 1 -: landLeft 2     -- (3, 1)

-- Negative numbers indicate flying off from the pole 
poleEx3 = (0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)
-- poleEx3 (1, 0) -> (1, 4) -> (0, 4) -> (0, 2) 
-- At 3rd landing, it should have toppled the pole!!

poleEx4 = (0, 0) -: landLeft 10             -- (10, 3)
-- This too, should have toppled.

-- So, we need landLeft and landRight to handle the case
landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left, right)
    | abs ((left + n) - right) <= 3 = Just (left + n, right)
    | otherwise                     = Nothing
    
landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left, right)
    | abs (left - (right + n)) <= 3 = Just (left, right + n)
    | otherwise                     = Nothing

-- See that, instead of returning 'Pole', these return 'Maybe Pole' taking care
-- of the failure condition
-- Nothing indicates the failure condition (the pole tumbles)

poleEx5 = landLeft' 2 (0, 0)                    -- Just (2, 0)
poleEx6 = landRight' 10 (0, 0)                  -- Nothing -- not Just (0, 10)

-- Let us do a sequence of landings and fly-offs
poleEx7 = return (0, 0) >>= landRight' 2 >>= landLeft' 2 >>= landRight' (-2)
-- This is very similar to poleEx3 example given above
-- Let us do the same using Monad 
poleEx8 = return (0,0) >>= landLeft' 1 >>= 
                            landRight' 4 >>= landLeft' (-1) >>= landRight' (-2)
        -- Nothing

-- poleEx8 is a very nice example of how one execution feeds into the other
-- using the "bind" (>>=) function / operator.  Made possible by the use of 
-- "Maybe" as a monad.

-- Now, a function for ignoring (making the pole tumble, irrespective of birds)
banana :: Pole -> Maybe Pole 
banana _ = Nothing

poleEx9 = return (0,0) >>= landLeft' 1 >>= banana >>= landRight' 2 -- Nothing
-- Here, Just (1,0) is fed into banana, but we know banana ignores whatever
-- that is fed in.  So, instead of using (>>=), we can use (>>)

{-
Recall on (>>)

(>>) :: Monad m => m a -> m b -> m b
m >> n = m >>= (\_ -> n)

(Nothing >> Just 4)     == Nothing
    Nothing >> Just 4
    Nothing >>= (\_ -> Just 4)
    Nothing
    
(Just 8 >> Nothing)     == Nothing 
(Just 5 >> Just 6)      == Just 6 
-}

-- Replacing (>>=) banana with (>>) Nothing - both are essentially the same 
poleEx10 = return (0,0) >>= landLeft' 1 >> Nothing >>= landRight' 2 -- Nothing

-- Now, to do the routine task, instead of making use of the monad 
-- Just to see how complicated and tedious the code turns out to be.
-- L1, R4, L2, L1 
routine :: Maybe Pole 
routine = case landLeft' 1 (0,0) of 
    Nothing -> Nothing
    Just pole1 -> case landRight' 4 pole1 of
        Nothing -> Nothing 
        Just pole2 -> case landLeft' 2 pole2 of
            Nothing -> Nothing 
            Just pole3 -> landLeft' 1 pole3

smart = return (0,0) >>= landLeft' 1 >>= landRight' 4 
                     >>= landLeft' 2 >>= landLeft' 1 
    -- Just (4,4)

-- (>>=) helps in doing successive computations that might have failed! 
    
{-- 

"do" notation

Introduced in IO context.  But, not particular to IO alone.
It can be used for any monad 

-}


monadEx1 = Just 3 >>= (\x -> Just (show x ++ "!"))              -- Just "3!"
monadEx2 = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

-- Without using the monad:
monadLet1 = let x = 3; y = "!" in show x ++ y               -- "3!"

-- Rewriting monadEx2
monadEx3 = Just 3   >>= (\x -> 
           Just "!" >>= (\y -> 
           Just (show x ++ y)))

-- Rewriting the same in do notation 
-- Helps in removing the unnecessary and non-readable lambda notations 
monadDo2 = do
    x <- return 3
    y <- return "!"
    Just (show x ++ y)

-- or, in particular 
monadDo3 = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)
    
{-

"do" notation
Every line is a monadic value 
Just to inspect the result (<-) is used. The results are temporarily 
extracted to the names on the left side 

-}

-- smart = return (0,0) >>= landLeft' 1 >>= landRight' 4 
--                     >>= landLeft' 2 >>= landLeft' 1 
     
smartWithDo = do
    start <- Just (0,0)
    first <- landLeft' 1 start 
    second <- landRight' 4 first
    third <- landLeft' 2 second 
    landLeft' 1 third 
-- Just (4,4)
    
-- So, 'do' notation, with help of monad, makes code looks imperative (executing
-- one line after another.  Rather, it is just sequential 

smartWithDo1 = do
    start <- Just (0,0)
    first <- landLeft' 1 start 
    second <- landRight' 4 first
    Nothing
    third <- landLeft' 2 second 
    landLeft' 1 third 
-- Nothing 
-- failure case is handled using Maybe Nothing 

-- Pattern matching in "do" 
justFirst :: [a] -> Maybe a
justFirst str = do
    (x:xs) <- Just str
    return x 
    
-- justFirst can be used as "safeHead"
-- when the pattern match fails for [], the following is taken care of 
-- "Nothing" will be the result

{-
5.

fail :: (Monad m) => String -> m a  
fail msg = error msg  

-- the above is the default implementation for "fail"

For Maybe, the implementation is overwritten with 
fail _ = Nothing 

-}


















































