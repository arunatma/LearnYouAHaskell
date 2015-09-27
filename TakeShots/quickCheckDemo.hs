import Test.QuickCheck
import Data.List

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs
          
          
prop_idempotent xs = qsort (qsort xs) == qsort xs

prop_minimum xs         = head (qsort xs) == minimum xs

-- with a not null constraint -> This will pass in quickCheck
prop_minimum' xs         = not (null xs) ==> head (qsort xs) == minimum xs          

prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_maximum xs         =
    not (null xs) ==>
        last (qsort xs) == maximum xs

prop_append xs ys       =
    not (null xs) ==>
    not (null ys) ==>
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)
        
-- Testing against a base function (here sort)        
prop_sort_model xs      = sort xs == qsort xs        

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show,Eq)
         

class Arbitrary a where
    arbitrary   :: Gen a
    elements :: [a] -> Gen a
    choose   :: Random a => (a, a) -> Gen a
    oneof    :: [Gen a] -> Gen a  
    
    
data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq,Show)


instance Arbitrary Ternary where
  arbitrary     = elements [Yes, No, Unknown]



instance Arbitrary Ternary where
  arbitrary     = do
      n <- choose (0, 2) :: Gen Int
      return $ case n of
                    0 -> Yes
                    1 -> No
                    _ -> Unknown

                    