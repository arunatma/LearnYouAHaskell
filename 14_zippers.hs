-- Learn You a Haskell For Great Good
-- Chapter 14: Zippers 
-- http://learnyouahaskell.com/zippers

-- How to change a particular node in a relatively complex tree?
-- In imperative languages, usually, access the memory and change the value.
-- Here, we need to create another tree which is similar to original tree and
-- but only, slightly different, with a single value changed.

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

charTree :: Tree Char
charTree = Node 'P' 
                (Node 'O'
                      (Node 'L'
                            (Node 'N' Empty Empty)
                            (Node 'T' Empty Empty)
                      )
                      (Node 'Y'
                            (Node 'S' Empty Empty)
                            (Node 'A' Empty Empty)
                      )
                )
                (Node 'L'
                      (Node 'W'
                            (Node 'C' Empty Empty)
                            (Node 'R' Empty Empty)
                      )
                      (Node 'A'
                            (Node 'A' Empty Empty)
                            (Node 'C' Empty Empty)
                      )
                )
                
-- change 'W' to 'Z'
changeToZ :: Tree Char -> Tree Char  
changeToZ (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'Z' m n) r)  
-- To change one node, we need to give almost entire tree structure, and get 
-- the same back, with the value changed.

-- Instead, let's start with the root node, and give a list of directions to 
-- access a particular node.
data Direction = L | R deriving (Show)  
type Directions = [Direction]  

-- Let us try changing the same 'W', but now instead with value 'M'  
changeToM :: Directions-> Tree Char -> Tree Char  
changeToM (L:ds) (Node x l r) = Node x (changeToM ds l) r  
changeToM (R:ds) (Node x l r) = Node x l (changeToM ds r)  
changeToM [] (Node _ l r) = Node 'M' l r  

-- To see, whether changeToM works fine, let us find out the element at a 
-- given position.
elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node x l r) = elemAt ds l
elemAt (R:ds) (Node x l r) = elemAt ds r
elemAt [] (Node x _ _) = x

newTree = changeToM [R, L] charTree
origChar = elemAt [R, L] charTree           -- 'W'
chngChar = elemAt [R, L] newTree            -- 'M'

-- Now, with a list of directions, we are able to change the node
-- But, in a bigger tree, we come all the way from top to change a bottom node.
-- Then, if we want to change an adjacent value, again, we need to come from top
-- The focus (or the starting point) is always the root at top, in this method.

-- How?
-- We need to leave traces, with respect to which direction we went!

type Breadcrumbs = [Direction]

-- We just save the left sub tree, if we have gone left. And save the right
-- sub tree if gone right
goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)

-- In our charTree, get the smallest tree containing "WCR"
wcrTree = goLeft (goRight (charTree, []))
-- wcrTree contains the tree, also the path to take from charTree to get wcrTree

-- to remove parenthesis, just as we defined in ch 12 (monads)
(-:) :: a -> (a -> b) -> b
x -: f = f x

treeWithCrumbs = (charTree, []) -: goRight -: goLeft    -- same as wcrTree

-- Again, with this, we will not be able to go up, because we do not have any
-- context or the info about other parts of trees.  What we know here is that
-- we got to this treeWithCrumbs by taking a 'Right' and then a 'Left'

-- We need to define a new data type to save the information
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
-- see this in contrast with the direction which is L | R
-- So, instead of just taking the direction, we are taking the element (a) 
-- along with the tree that we did not traverse (Tree a)

type BrdCrmbs a = [Crumb a]

goLeft' :: (Tree a, BrdCrmbs a) -> (Tree a, BrdCrmbs a)
goLeft' (Node x l r, bs) = (l, (LeftCrumb x r) : bs)

goRight' :: (Tree a, BrdCrmbs a) -> (Tree a, BrdCrmbs a)
goRight' (Node x l r, bs) = (r, (RightCrumb x l) : bs)

-- Now, we got the information needed to go up in a tree
goUp' :: (Tree a, BrdCrmbs a) -> (Tree a, BrdCrmbs a)
goUp' (t, (LeftCrumb x r) : bs) = (Node x t r, bs)
goUp' (t, (RightCrumb x l) : bs) = (Node x l t, bs)

