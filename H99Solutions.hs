module H99Solutions where
import Data.List ( group )

myLast :: [a] -> a
myLast = head . reverse

myButLast :: [a] -> a
myButLast = myLast . init

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt [] _ = error "Index error"
elementAt (_:xs) n
  | n < 1      = error "Index error"
  | otherwise  = elementAt xs (n-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ (listify x)
  where
    listify x = [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = (head xs) == (last xs) && (isPalindrome . tail . init) xs

data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List x) = concatMap myFlatten x

myCompress :: String -> String
myCompress xs = map head (group xs)
