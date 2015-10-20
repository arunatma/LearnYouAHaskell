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
