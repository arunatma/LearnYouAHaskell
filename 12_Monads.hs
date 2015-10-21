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
    | abs ((left - (right + n) <= 3 = Just (left, right + n)
    | otherwise                     = Nothing

-- See that, instead of returning 'Pole', these return 'Maybe Pole' taking care
-- of the failure condition

