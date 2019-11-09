module H99Solutions where

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
