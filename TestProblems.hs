import Test.HUnit
import H99Solutions

test1   = TestCase (assertEqual "Solution1 does not work" 4   (myLast [1,2,3,4]))
test2   = TestCase (assertEqual "Solution2 does not work" 3   (myButLast [1,2,3,4]))
test3_1 = TestCase (assertEqual "Solution3 does not work" 2   (elementAt [1,2,3] 2))
test3_2 = TestCase (assertEqual "Solution3 does not work" 'e' (elementAt "haskell" 5))

tests = TestList [ TestLabel "solution1" test1,
                   TestLabel "solution2" test2,
                   TestLabel "solution2" test3_1,
                   TestLabel "solution2" test3_2]

main::IO Counts
main = do
    runTestTT $ tests
