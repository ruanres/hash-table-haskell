{-
	Tests for Hash table open address linear probing
-}

import HashTableOpenAddressLinearProbing
import Test.HUnit
import qualified Data.Map as M

--Test for Size

test0 = TestCase (assertEqual "Size" 3 size)

testsSize= TestList [TestLabel "test0" test0]

--Test for Insert 

listaVazia = M.empty

test1 = TestCase (assertEqual "Lista vazia," 0 (M.size listaVazia))

listaUmElemento = insert "batata" listaVazia

test2 = TestCase (assertEqual "Lista com 1 elemento," (M.fromList [(0,"batata")]) listaUmElemento)

listaDoisElementos = insert "casca" listaUmElemento

test3 = TestCase (assertEqual "Lista com 2 elementos," (M.fromList [(0,"batata"),(2,"casca")]) listaDoisElementos) 

testsInsert = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3]

--Test for Remove


listaSemBatata = remove "batata" listaDoisElementos

test4 = TestCase (assertEqual "Remove batata," (M.fromList [(0,"DELETED_SLOT"),(2,"casca")]) listaSemBatata)

listaSemCasca = remove "casca" listaSemBatata

test5 = TestCase (assertEqual "Remove casca," (M.fromList [(0,"DELETED_SLOT"),(2,"DELETED_SLOT")]) listaSemCasca)

testsRemove = TestList [TestLabel "test4" test4, TestLabel "test5" test5]

--Test for Contain

test6 = TestCase (assertBool"Contém batata," (contain "batata" listaUmElemento))

test7 = TestCase (assertBool "Contém casca," (contain "batata" listaDoisElementos))

test8 = TestCase (assertBool "Nao contém batata," (not(contain "batata" listaSemBatata)))

test9 = TestCase (assertBool "Nao contém casca," (not(contain "batata" listaSemCasca)))

testsContain = TestList [TestLabel "test6" test6,TestLabel "test7" test7,TestLabel "test8" test8,TestLabel "test9" test9]

--Test for search

test10 = TestCase (assertEqual "Procura batata," 0 (search "batata" listaUmElemento))

test11 = TestCase (assertEqual "Procura Casca," 2 (search "casca" listaDoisElementos))

test12 = TestCase (assertEqual "Procura batata," (-1) (search "batata" listaSemBatata))

test13 = TestCase (assertEqual "Procura Casca," (-1) (search "casca" listaSemCasca))

testsSearch = TestList [TestLabel "test10" test10, TestLabel "test11" test11,TestLabel "test12" test12,TestLabel "test13" test13]


--Test for hash

test14 = TestCase (assertEqual "Hash batata," 0 (hash "batata" 0))

test15 = TestCase (assertEqual "Hash casca," 2 (hash "casca" 0))

testsHash = TestList [TestLabel "test14" test14, TestLabel "test15" test15]

--Test for CurrentSize

test16 = TestCase (assertEqual "Um elemnto," 1 (currentSize listaUmElemento))

test17 = TestCase (assertEqual "Dois elementos," 2 (currentSize listaDoisElementos))

test18 = TestCase (assertEqual "Um elemento," 1 (currentSize listaSemBatata))

test19 = TestCase (assertEqual "Vazia," 0 (currentSize listaSemCasca))

testsCurrentSize = TestList [TestLabel "test16" test16, TestLabel "test17" test17, TestLabel "test18" test18, TestLabel "test19" test19]

--Test for isFull

test20 = TestCase (assertBool"Nao esta cheia" (not(isFull listaUmElemento)))

test21 = TestCase (assertBool "Nao esta cheia" (not(isFull listaDoisElementos)))

listaTresElementos = insert "coco" listaDoisElementos

test22 = TestCase (assertBool "Esta Cheia," (isFull listaTresElementos))

testsIsFull = TestList [TestLabel "test20" test20, TestLabel "test21" test21, TestLabel "test22" test22]
