module H99Solutions where
import Data.List ( group )
import Data.List.Split ( chunksOf )

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

myPack :: [Char] -> [String]
myPack xs = ["aaaa","b","cc","aa","d","eeee"]

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\s -> (length s, head s)) . group

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map listItemize . encode
    where
      listItemize (1, x) = Single x
      listItemize (n, x) = Multiple n x

instance Eq a => Eq (ListItem a) where
    (Single x) == (Single y) = x == y
    (Multiple n x) == (Multiple m y) = x == y && n == m
    _ == _ = False

decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
    where
        decodeHelper (Single x) = [x]
        decodeHelper (Multiple n x) = replicate n x

-- | Run Length Decoding
--
-- >>> decode [(3,'a'),(2,'b')]
-- "aaabb"
decode :: Eq a => [(Int, a)] -> [a]
decode = concatMap rl2str

-- | Run Length Decoding
--
-- >>> rl2str (3,'a')
-- "aaa"
rl2str :: Eq a => (Int, a) -> [a]
rl2str (n,c) = replicate n c

dupli :: Eq a => [a] -> [a]
dupli = concatMap (replicate 2)

repli :: Eq a => [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

dropEvery :: Eq a => [a] -> Int -> [a]
dropEvery xs n = concatMap dropHelpler (chunksOf n xs)
    where
        dropHelpler xs
            | length xs == n = init xs
            | otherwise      = xs
