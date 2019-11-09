import Test.HUnit
import H99Solutions

test1   = TestCase (assertEqual "Solution1 does not work" 4   (myLast [1,2,3,4]))

test2   = TestCase (assertEqual "Solution2 does not work" 3   (myButLast [1,2,3,4]))

test3_1 = TestCase (assertEqual "Solution3 does not work" 2    (elementAt [1,2,3] 2))
test3_2 = TestCase (assertEqual "Solution3 does not work" 'e' (elementAt "haskell" 5))

test4_1 = TestCase (assertEqual "Solution4 does not work"  3  (myLength [123, 456, 789]))
test4_2 = TestCase (assertEqual "Solution4 does not work" 13 (myLength "Hello, world!"))

test5_1 = TestCase (assertEqual "Solution5 does not work" [4,3,2,1] (myReverse [1,2,3,4]))

test6_1 = TestCase (assertEqual "Solution6 does not work" False (isPalindrome [1,2,3]))
test6_2 = TestCase (assertEqual "Solution6 does not work" True (isPalindrome "madamimadam"))
test6_3 = TestCase (assertEqual "Solution6 does not work" True (isPalindrome [1,2,4,8,16,8,4,2,1]))

tests = TestList [ TestLabel "solution1" test1,
                   TestLabel "solution2" test2,
                   TestLabel "solution3" test3_1,
                   TestLabel "solution3" test3_2,
                   TestLabel "solution4" test4_1,
                   TestLabel "solution4" test4_2,
                   TestLabel "solution5" test5_1,
                   TestLabel "solution6" test6_1]
--                   TestLabel "solution6" test6_2,
--                   TestLabel "solution6" test6_3]

main::IO Counts
main = do
    runTestTT $ tests
