-- Learn You a Haskell For Great Good
-- Chapter 10: Functionally Solving Problems
-- http://learnyouahaskell.com/functionally-solving-problems

import Data.List  

{-------------------------------------------------------------------------------
                    Problem 1: Reverse Polish Notation Calculator
                    
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
            Here, b is [Int]  		
                  a is a single String 	i.e. [Char]
			foldingFunction takes a stack and an item, returns a stack
    7. Same function, in point-free style
        solveRPN :: (Num a) => String -> a  
        solveRPN  = head . foldl foldingFunction [] . words  
            where   foldingFunction stack item = ... 
    8. Constructiong the foldingFunction for only "*", "+" and "-"

-------------------------------------------------------------------------------}

solveRPN :: (Num a, Read a) => String -> a  
solveRPN  = head . foldl foldingFunction [] . words  

-- Step 8: Construction the foldingFunction:
foldingFunction :: (Num a, Read a) => [a] -> String -> [a]
foldingFunction (x:y:ys) "*" = (x * y) : ys
foldingFunction (x:y:ys) "+" = (x + y) : ys
foldingFunction (x:y:ys) "-" = (y - x) : ys
foldingFunction xs numberString = (read numberString) : xs   


solveRPN' ::  String -> Double  
solveRPN' = head . foldl foldFn [] . words 

-- Including a few other operators in the fold function.
foldFn :: (Floating a, Read a) => [a] -> String -> [a]
foldFn (x:y:xs) "*" = (x * y) : xs
foldFn (x:y:xs) "+" = (x + y) : xs
foldFn (x:y:xs) "-" = (y - x) : xs
foldFn (x:y:ys) "/" = (y / x) : ys  
foldFn (x:y:ys) "^" = (y ** x): ys  
foldFn (x:xs) "ln"  = log x   : xs  
foldFn xs     "sum" = [sum xs]  
foldFn xs numberString = read numberString:xs

ex1 = solveRPN' "2 4 +"                         -- 6
ex2 = solveRPN' "10 10 10 10 10 sum 4 /"        -- 12.5
ex3 = solveRPN' "2.7 ln"                        -- 0.99325
ex4 = solveRPN' "2 3 ^"                         -- 8

{-------------------------------------------------------------------------------
                        Problem 2: Heathrow to London
                        
    Airport has to points to get out, to go to London (A and B).  At any 
    junction, any new path (straight or across) can be taken.
    Find the shortest path.
                        
                50  A1    5      A2    40     A3      10    
           A--------|------------|------------|--------------A4
                    |            |            |               |  
    AIRPORT         |30          |20          |25             |0      LONDON
                    |            |            |               |    
           B--------|------------|------------|--------------B4
                10  B1    90     B2     2     B3      8
            
    In this case, the shortest path is 
        10 -> 30 -> 5 -> 20 -> 2 -> 8       = 75 km
        
Objective:
    Take input of the road-paths in a specified format and print out the 
    shortest path with the total shortest distance to travel
Input format:
    The input format for the above grid would be 
        50
        10
        30
        5
        90 
        20
        40 
        2
        25 
        10
        8
        0
        
        The input format is nothing but, road A distance to next node, road B 
        distance to next node, followed by the interconnection distance between
        the nodes.
        
    Before jumping on to design and solve the problem in Haskell, it is better
    to understand and solve problem manually on a notebook, get a pattern
    and proceed to implement a similar logic using Haskell.
    
Manual Analysis:
    Let us find out the shortest path to the intermediate nodes:
    'C' represents a cross over when you cross from A to B or B to A 
    A1:     B -> B1 -> A1   = 40    (shortest)      B and Cross Over (B, C)
            A -> A1         = 50 
            
    B1:     B -> B1         = 10    (shortest)      B                 (B)
            A -> A1 -> B1   = 80
            
    A2:     A1 -> A2        = 45    (shortest)      From Start: (B, C, A)     
            B1 -> B2 -> A2  = 120
        
    B2:     B1 -> B2        = 100 
            A1 -> A2 -> B2  = 65    (shortest)      From Start: (B, C, A, C)

So, find the shortest path @ each cross road and going forward, utilize the 
same to find the further shorter paths.

Need to maintain the shortest path along all nodes on Road A and Road B

Now, representing the road system in Haskell.            
-------------------------------------------------------------------------------}

-- A node can have two roads attached to it, or a terminal node
data Node = Node Road Road | EndNode Road

-- How long it is and which node it points to 
data Road = Road Int Node 

-- Another way of implementation 
-- A node can have one road, or two roads.
data Node' = Node' Road' (Maybe Road')
data Road' = Road' Int Node'

-- Instead of dealing with nodes and roads, which are too general, in our 
-- specific case, we always dealt with a section comprising road A, road B and 
-- the cross road C 
data Section = Section { getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem 
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, 
                    Section 10 8 0]

-- To implement a path                     
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

-- The final function, should take the road system and give out the optimal 
-- path (labelling the road along with the distance of the road)
-- So the type signature to be 
-- optimalPath :: RoadSystem -> Path
-- The result for the given problem should be
-- [(B, 10), (C, 30), (A, 5), (C, 20), (B, 2), (B, 8)]


-- The repetitive step: Done for each section of the road system.

-- For the Section 50 10 30 (the first one)
-- Optimal path to A1 is [(B, 10), (C, 30)]
-- Optimal path to B1 is [(B, 10)] 

-- For any section to work on, we need to know the optimal paths of A and B 
-- nodes till the previous section.

-- So, this function should take the previous optimal paths for A and B roads 
-- and the current section and produce the optimal paths for A and B, 
-- incorporating the current section of the road.
roadStep :: (Path, Path) -> Section -> (Path, Path)  
roadStep (pathA, pathB) (Section a b c) =   
    let priceA = sum $ map snd pathA  
        priceB = sum $ map snd pathB  
        forwardPriceToA = priceA + a  
        crossPriceToA = priceB + b + c  
        forwardPriceToB = priceB + b  
        crossPriceToB = priceA + a + c  
        newPathToA = if forwardPriceToA <= crossPriceToA  
                        then (A, a) : pathA  
                        else (C, c) : (B, b) : pathB  
        newPathToB = if forwardPriceToB <= crossPriceToB  
                        then (B, b) : pathB  
                        else (C, c) : (A, a) : pathA  
    in  (newPathToA, newPathToB)

-- Examining the optimal paths for the left most section.    
firstSectionPaths = roadStep ([], []) (head heathrowToLondon)
-- the paths are given in the reverse direction, because we start with an 
-- empty list and keep pushing the best segment to it, from left     
    
-- Optimal path for the entire road system is obtained, by processing 
-- each section one after another, starting from the left. (using foldl)
optimalPath :: RoadSystem -> Path  
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem  
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)  
            then reverse bestAPath  
            else reverse bestBPath     
            
bestPath = optimalPath heathrowToLondon
-- [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]

-- Now to make it work, with the given input, as represented above in a text 
-- file (or from the standard input)

-- groupsOf function takes an integer and a list and then make a list of lists.
-- where the inside lists are groups of 'n' the number which we have input.
groupsOf :: Int -> [a] -> [[a]]  
groupsOf 0 _ = undefined  
groupsOf _ [] = []  
groupsOf n xs = take n xs : groupsOf n (drop n xs)
  
-- In this roadsystem example, we need groups of 3.  
processPath = do  
    contents <- getContents  
    let threes = groupsOf 3 (map read $ lines contents)  
        roadSystem = map (\[a,b,c] -> Section a b c) threes  
        path = optimalPath roadSystem  
        pathString = concat $ map (show . fst) path  
        pathPrice = sum $ map snd path  
    putStrLn $ "The best path to take is: " ++ pathString  
    putStrLn $ "The price is: " ++ show pathPrice
    
main = processPath
    
-- Use this .hs file and paths.txt to run this example    
{-
    > cat paths.txt | runhaskell 10_Solving_Problems.hs    (linux)
    > type paths.txt | runhaskell 10_Solving_Problems.hs   (windows)   
    The best path to take is: BCACBBC  
    The price is: 75      
-}
