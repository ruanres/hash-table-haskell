{-
	Test for Hash table closed address
-}

import HashTableClosedAddress
import Test.HUnit
import qualified Data.Map as M

--Tests for insert

listaVazia = M.empty

test0 = TestCase (assertEqual "Lista vazia," 0 (M.size listaVazia))

listaUmElemento = insert "batata" listaVazia

test1 = TestCase (assertEqual "Lista com 1 elemento," (M.fromList [(6,["batata"])]) listaUmElemento)

listaDoisElementos = insert "casca" listaUmElemento

test2 = TestCase (assertEqual "Lista com 2 elementos," (M.fromList [(5,["casca"]),(6,["batata"])]) listaDoisElementos) 

testsInsert = TestList [TestLabel "test0" test0, TestLabel "test1" test1, TestLabel "test2" test2]

--Tests for remove


test3 = TestCase (assertEqual "Size" 30 size)

listaSemBatata = remove "batata" listaDoisElementos

test4 = TestCase (assertEqual "Remove batata," (M.fromList [(5,["casca"]),(6,[])]) listaSemBatata)

listaSemCasca = remove "casca" listaSemBatata

test5 = TestCase (assertEqual "Remove casca," (M.fromList [(5,[]),(6,[])]) listaSemCasca)

testsRemove = TestList [TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5 ]

--Test for contain

test6 = TestCase (assertBool"Contém batata," (contain "batata" listaUmElemento))

test7 = TestCase (assertBool "Contém casca," (contain "batata" listaDoisElementos))

test8 = TestCase (assertBool "Nao contém batata," (not(contain "batata" listaSemBatata)))

test9 = TestCase (assertBool "Nao contém casca," (not(contain "batata" listaSemCasca)))

testsContain = TestList [TestLabel "test6" test6,TestLabel "test7" test7,TestLabel "test8" test8,TestLabel "test9" test9]

--Test for search

test10 = TestCase (assertEqual "Procura batata," 6 (search "batata" listaUmElemento))

test11 = TestCase (assertEqual "Procura Casca," 5 (search "casca" listaDoisElementos))

test12 = TestCase (assertEqual "Procura batata," (-1) (search "batata" listaSemBatata))

test13 = TestCase (assertEqual "Procura Casca," (-1) (search "casca" listaSemCasca))


testsSearch = TestList [TestLabel "test10" test10, TestLabel "test11" test11,TestLabel "test12" test12,TestLabel "test13" test13]

--Test for hash

test14 = TestCase (assertEqual "Hash batata," 6 (hash "batata"))

test15 = TestCase (assertEqual "Hash casca," 5 (hash "casca"))

testsHash = TestList [TestLabel "test14" test14, TestLabel "test15" test15]




 



