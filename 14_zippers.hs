-- Learn You a Haskell For Great Good
-- Chapter 14: Zippers 
-- http://learnyouahaskell.com/zippers

-- How to change a particular node in a relatively complex tree?
-- In imperative languages, usually, access the memory and change the value.
-- Here, we need to create another tree which is similar to original tree and
-- but only, slightly different, with a single value changed.

-- needed for File System example.
import Data.List (break)

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
-- goLeft' gives a Tree and BrdCrmbs as a tuple
-- If you start at the root, the first element of goLeft' is the tree which 
-- is arrived taking a left from the root and the BrdCrmbs list will have a 
-- single element of data type "Crumb a", which is LeftCrumb (rootNode) (tree
-- formed if taken not taken left)

goRight' :: (Tree a, BrdCrmbs a) -> (Tree a, BrdCrmbs a)
goRight' (Node x l r, bs) = (r, (RightCrumb x l) : bs)

-- Now, we got the information needed to go up in a tree
goUp' :: (Tree a, BrdCrmbs a) -> (Tree a, BrdCrmbs a)
goUp' (t, (LeftCrumb x r) : bs) = (Node x t r, bs)
goUp' (t, (RightCrumb x l) : bs) = (Node x l t, bs)

{-------------------------------------------------------------------------------
  goUp' takes a tuple containing two elements: the current tree and the 
  list of Crumbs i.e BrdCrmbs. Each Crumb contain information about the element
  which is just the parent of the current tree and the tree which is not taken.
  So, to get a tree at one step up, form a tree using the first element from 
  BrdCrmbs representing the root node of the current tree and the LeftCrumb
  or RightCrumb (the tree that is not traversed with the given root node)
-------------------------------------------------------------------------------}

-- Todo:
-- Failure case not handled for goUp' if already at the top most node
-- Need to implement using 'Maybe'

{-------------------------------------------------------------------------------

    So, (Tree a, BrdCrmbs a) contain information about a particular node and
    its parent, along with its other child.  So, the focus is exactly on the 
    node being examined, with complete information about its surroundings
    
    Such, a focussed sub part of a complex data structures is Zipper
-------------------------------------------------------------------------------}

type Zipper a = (Tree a, BrdCrmbs a)

-- Manipulating the trees under focus
-- Apply a function on a given node.
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

-- From top, go left, go right replace the node to 'M'
newFocus = modify (\_ -> 'M') (goRight' (goLeft' (charTree, [])))
-- Here the focus will be on the node obtained by turning a left and right
-- But, the breadcrumbs has information about the entire tree.

-- the same, using (-:)
sameNewFocus = (charTree, []) -: goLeft' -: goRight' -: modify (\_ -> 'M')

-- Let us get on to another focus, by going one up
newFocus2 = goUp' newFocus

-- Change the element at newFocus2 to something else (say 'X')
changedNewFocus2 = modify (\_ -> 'X') newFocus2

-- same using (-:), directly from newFocus, instead of getting from newFocus2
sameChangedNewFocus2 = newFocus -: goUp' -: modify (\_ -> 'X') 

{-------------------------------------------------------------------------------
    Attaching a tree, at a given node
-------------------------------------------------------------------------------}

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)
-- Effectively means:
-- Focus on a given node (getting a Zipper); Do not care about what is there in 
-- the node; get a tree and attach there, and present the entire tree as Zipper

farLeft = (charTree, []) -: goLeft' -: goLeft' -: goLeft' -: goLeft'
zipperExtTree = farLeft -: attach (Node 'X' Empty Empty)
-- zipperExtTree contains entire tree information with the new small tree
-- attached at the far left of the charTree

-- Use recursion to get the top most node
topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])               -- If bs empty, we are already at top!
topMost z       = topMost (goUp' z)     -- Else, go up once!

{-------------------------------------------------------------------------------
                    Zippers for Lists
    Idea is to traverse through a list and remember the path which traversed.
-------------------------------------------------------------------------------}
-- A sample data type for implementing List
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- The following list is same as 1:2:3:4:[] or 1:2:3:[4] or 1:[2,3,4]
listEx1 = [1, 2, 3, 4]

listEx2 = head listEx1 : tail listEx1           -- 1:[2,3,4] same as listEx1

-- To make a zipper for lists, we need to have the list element along with 
-- other elements following it in focus, with what we have not selected as 
-- breadcrumbs

-- The first one is the list which is in focus, the second one is the crumbs
type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)       -- Take head of list and put in crumbs

goBack :: ListZipper a -> ListZipper a 
goBack (xs, b:bs) = (b:xs, bs)          -- Take head of crumbs & attach to list.

xs = listEx1 
lzip1 = goForward (xs, [])              -- ([2,3,4], [1])
lzip2 = goForward lzip1                 -- ([3,4], [2,1])
lzip3 = goForward lzip2                 -- ([4], [3,2,1])

lzip4 = goBack lzip3                    -- ([3,4], [2,1]) i.e lzip2

-- The breadcrumbs are just the revers of the path that we travelled through 
-- in the list. The recent travelled comes first in the breadcrumb.

-- List Zippers can be used in text editors to store line strings and to get 
-- the focus on the current line where the cursor is on.

{-------------------------------------------------------------------------------
                    File System using Zippers.
-------------------------------------------------------------------------------}
-- File: simplest unit of data, with an identifiable name.
-- Folder: Collection of files and other foldrers; It too has a name.

type Name = String                          -- name of a file
type Data = String                          -- data contents of a file 

-- A item of file system can be a file or a folder.
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

-- Let us create a sample file system (starting with folder name root)
curDisk :: FSItem  
curDisk = 
    Folder "root"   
        [ File "file1.txt" "text contents"  
        , File "file2.bin" "assume binary contents"  
        , Folder "pics"  
            [ File "pic1.jpg" "assume jpg file."  
            , File "pic2.gif" "assume gif file."  
            , File "pic3.bmp" "assume bmp file."  
            ]  
        , File "doc1.doc" "Possibly MS Word"  
        , Folder "programs"  
            [ File "app1.exe" "assume exe file"  
            , File "test.dmg" "assume some dmg file"  
            , File "app2.exe" "assume exe file"  
            , Folder "source code"  
                [ File "best_hs_prog.hs" "main = print (fix error)"  
                , File "random.hs" "main = print 4"  
                ]  
            ]  
        ]  

-- A file system can just be thought of a tree (but with multiple nodes)
-- This again is a hierarchial structure.

-- Zipper for the FSItem
-- To get the entire context, when the focus is on a single item, we need to 
-- know the parent folder (if it is a file) or the folder itself, along with 
-- the structure that is up, and the structure that is down in hierarchy.

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

-- type synonym for the file system zipper.
type FSZipper = (FSItem, [FSCrumb])

-- going up in a file system zipper.
-- Get the first breadcrumb; All items preceding the focus and all items 
-- following the focus (say, a single file) are in separate lists in that 
-- breadcrumb. use it to get the folder information.

-- Given a file, fsUp will point to the folder it is present in. 
-- (and given a folder, it would point to the parent folder)
fsUp :: FSZipper -> FSZipper
fsUp (item, (FSCrumb name ls rs) : bs) = (Folder name (ls ++ [item] ++ rs), bs)

-- Going to a specific file, collecting information about the path.
-- Here, the assumption is the file we are searching on is in current folder.
-- fsTo does not search across the folders.
fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) = 
    let (ls, item:rs) = break (nameIs name) items
    in  (item, (FSCrumb folderName ls rs) : bs)
    
nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName  
nameIs name (File fileName _) = name == fileName

-- break function is from "Data.List"