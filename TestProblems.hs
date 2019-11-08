import Test.HUnit
import H99Solutions

test1 = TestCase (assertEqual "myLast [1,2,3,4]" 4 (myLast [1,2,3,4]))
test2 = TestCase (assertEqual "myButLast [1,2,3,4]" 3 (myButLast [1,2,3,4]))

tests = TestList [ TestLabel "solution1" test1,
                   TestLabel "solution2" test2]

main::IO Counts
main = do
    runTestTT $ tests
