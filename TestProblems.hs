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
test7_1 = TestCase (assertEqual "Solution7 does not work" [1,2,3,4,5] (myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])))
test8_1 = TestCase (assertEqual "Solution8 does not work" "abcade" (myCompress "aaaabccaadeeee"))
test8_2 = TestCase (assertEqual "Solution8 does not work" "" (myCompress ""))
test9_1 = TestCase (assertEqual "Solution8 does not work" ["aaaa","b","cc","aa","d","eeee"] (myPack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']))
test10_1 = TestCase (assertEqual "Solution10 does not work" [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')] (encode "aaaabccaadeeee"))
test11_1 = TestCase (assertEqual "Solution11 does not work" [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e'] (encodeModified "aaaabccaadeeee"))
test12_1 = TestCase (assertEqual "Solution12 does not work" "aaaabccaadeeee" (decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']))
test14_1 = TestCase (assertEqual "Solution14 does not work" [1,1,2,2,3,3] (dupli [1,2,3]))
test15_1 = TestCase (assertEqual "Solution15 does not work" "aaabbbccc" (repli "abc" 3))
test16_1 = TestCase (assertEqual "Solution16 does not work" "abdeghk" (dropEvery "abcdefghik" 3))
test17_1 = TestCase (assertEqual "Solution17 does not work" ("abc", "defghik") (split "abcdefghik" 3))
test18_1 = TestCase (assertEqual "Solution18 does not work" "cdefg" (slice ['a','b','c','d','e','f','g','h','i','k'] 3 7))
test19_1 = TestCase (assertEqual "Solution19-1 does not work" "defghabc" (rotate ['a','b','c','d','e','f','g','h'] 3))
test19_2 = TestCase (assertEqual "Solution19-2 does not work" "ghabcdef" (rotate ['a','b','c','d','e','f','g','h'] (-2)))
test20_1 = TestCase (assertEqual "Solution20-1 does not work" ('b',"acd") (removeAt 2 "abcd"))
test21_1 = TestCase (assertEqual "Solution21-1 does not work" "aXbcd" (insertAt 'X' "abcd" 2))
test22_1 = TestCase (assertEqual "Solution22-1 does not work" [4,5,6,7,8,9] (range 4 9))


tests = TestList [ TestLabel "solution1" test1,
                   TestLabel "solution2" test2,
                   TestLabel "solution3" test3_1,
                   TestLabel "solution3" test3_2,
                   TestLabel "solution4" test4_1,
                   TestLabel "solution4" test4_2,
                   TestLabel "solution5" test5_1,
                   TestLabel "solution6" test6_1,
                   TestLabel "solution6" test6_2,
                   TestLabel "solution6" test6_3,
                   TestLabel "solution7" test7_1,
                   TestLabel "solution8" test8_1,
                   TestLabel "solution8" test8_2,
                   TestLabel "solution9" test9_1,
                   TestLabel "solution10" test10_1,
                   TestLabel "solution11" test11_1,
                   TestLabel "solution12" test12_1,
                   TestLabel "solution14" test14_1,
                   TestLabel "solution15" test15_1,
                   TestLabel "solution16" test16_1,
                   TestLabel "solution17" test17_1,
                   TestLabel "solution18" test18_1,
                   TestLabel "solution19" test19_1,
                   TestLabel "solution19" test19_2,
                   TestLabel "solution20" test20_1,
                   TestLabel "solution21" test21_1,
                   TestLabel "solution22" test22_1]

main::IO Counts
main = do
    runTestTT $ tests
