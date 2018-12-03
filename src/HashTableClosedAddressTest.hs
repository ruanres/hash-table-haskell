module HashTableClosedAddressTest where

import Test.HUnit

import HashTableClosedAddress as HT


test01 = TestCase (assertEqual "test size," HT.size 10)

testHash01 = TestCase (assertEqual "test hash empty word" (HT.hash "") 0)
testHash02 = TestCase (assertEqual "test hash not empty word" (HT.hash "banana") 6)
testHash03 = TestCase (assertEqual "test hash word bigger than tabel size" (HT.hash "banana e caja") 3)







tests = TestList [
  test01, 
  testHash01, testHash02, testHash03
  ]

runTests :: IO Counts
runTests = do
    runTestTT tests