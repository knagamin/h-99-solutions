module H99Solutions where

myLast :: [a] -> a
myLast = head . reverse

myButLast :: [a] -> a
myButLast = myLast . init
