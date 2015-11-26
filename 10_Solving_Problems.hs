-- Learn You a Haskell For Great Good
-- Chapter 10: Functionally Solving Problems
-- http://learnyouahaskell.com/functionally-solving-problems

import Data.List  

{-------------------------------------------------------------------------------
                    Reverse Polish Notation Calculator
                    
Mathematical Expression:
        10 - (4 + 3) * 2
        
RPN Expression:
        10 4 3 + 2 * -

Execution of RPN Expression:
    1. Keep getting a number or operator from the left
    2. When an opertor is encountered, do the operation with the last found 2 
       numbers.
    3. Keep processing steps 1 and 2
    
            10                  : Get
            10 4                : Get 
            10 4 3              : Get
            10 4 3 +            : Get
            10 7                : Operator found; So, process!
            10 7 2              : Get
            10 7 2 *            : Get
            10 14               : Operator found; So, process!
            10 14 -             : Get
            -4                  : Operator found; So, process!
            -4                  : Final Result
            
Using a Stack:
    * If it is a number, push on to stack
    * If it is an operator, pop two of the recent numbers and execute.
    
    10 4 3 + 2 * -
            []                  : Initial Stack
        10  [10]                : Push 10
        4   [4,10]              : Push 4
        3   [3,4,10]            : Push 3
        +   [7,10]              : Found '+'; Pop 2 nums; Process; Push Result.
        2   [2,7,10]            : Push 2
        *   [14,10]             : Found '*'; Pop 2 nums; Process; Push Result.
        -   [-4]                : Found '-'; Pop 2 nums; Process; Push Result.
            [-4]                : Final Result
            
Designing a RPN Calculator:
    1. Input is String
    2. Output is a Number
        So, the type signature should be
            solveRPN :: (Num a) => String -> a.
    3. "10 4 3 + 2 * -" is to be converted to ["10","4","3","+","2","*","-"]
        Using 'words' function from Data.List - String -> [String] based on ' '
    4. As above, let us use a list for stack
        Push and pop happens on the left of the list (on the head side)
    5. We need to process a list of items in a list and make it a single value
        So, the usage of 'foldl' is needed - as we process from left.
    6. As of now, the function should look somewhat like:    
        solveRPN :: (Num a) => String -> a  
        solveRPN expression = head (foldl foldingFunction [] (words expression))  
            where   foldingFunction stack item = ... 
            
        The accumulator here is the empty list [] (see Initial Stack, above)
        
        :t foldl :: (b -> a -> b) -> b -> [a] -> b
            foldingFunction should be of type (b -> a -> b)
            Here, b is [String]
                  a is 
    7. Same function, in point-free style
        solveRPN :: (Num a) => String -> a  
        solveRPN  = head . foldl foldingFunction [] . words  
            where   foldingFunction stack item = ... 
            
-------------------------------------------------------------------------------}
  
  
solveRPN ::  String -> Double  
solveRPN = head . foldl foldFn [] . words 

foldFn :: (Floating a, Read a) => [a] -> String -> [a]
foldFn (x:y:xs) "*" = (x * y) : xs
foldFn (x:y:xs) "+" = (x + y) : xs
foldFn (x:y:xs) "-" = (y - x) : xs
foldFn (x:y:ys) "/" = (y / x) : ys  
foldFn (x:y:ys) "^" = (y ** x): ys  
foldFn (x:xs) "ln"  = log x   : xs  
foldFn xs     "sum" = [sum xs]  
foldFn xs numberString = read numberString:xs
