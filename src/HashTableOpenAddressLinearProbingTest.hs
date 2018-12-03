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

listaVaziaTest = TestCase (assertEqual "Lista vazia," 0 (M.size listaVazia))

listaUmElemento = insert "batata" listaVazia

listaUmElementoTest = TestCase (assertEqual "Lista com 1 elemento," (M.fromList [(0,"batata")]) listaUmElemento)

listaDoisElementos = insert "casca" listaUmElemento

listaDoisElementosTest = TestCase (assertEqual "Lista com 2 elementos," (M.fromList [(0,"batata"),(2,"casca")]) listaDoisElementos) 

testsInsert = TestList [TestLabel "listaVaziaTest" listaVaziaTest, TestLabel "listaUmElementoTest" listaUmElementoTest, TestLabel "listaDoisElementosTest" listaDoisElementosTest]

--Test for Remove


listaSemBatata = remove "batata" listaDoisElementos

listaSemBatataTest = TestCase (assertEqual "Remove batata," (M.fromList [(0,"DELETED_SLOT"),(2,"casca")]) listaSemBatata)

listaSemCasca = remove "casca" listaSemBatata

listaSemCascaTest = TestCase (assertEqual "Remove casca," (M.fromList [(0,"DELETED_SLOT"),(2,"DELETED_SLOT")]) listaSemCasca)

testsRemove = TestList [TestLabel "listaSemBatataTest" listaSemBatataTest, TestLabel "listaSemCascaTest" listaSemCascaTest]

--Test for Contain

contemBatataTest = TestCase (assertBool"Contém batata," (contain "batata" listaUmElemento))

contemCascaTest = TestCase (assertBool "Contém casca," (contain "casca" listaDoisElementos))

naoContemBatataTest = TestCase (assertBool "Nao contém batata," (not(contain "batata" listaSemBatata)))

naoContemCascaTest = TestCase (assertBool "Nao contém casca," (not(contain "casca" listaSemCasca)))

testsContain = TestList [TestLabel "contemBatataTest" contemBatataTest,TestLabel "contemCascaTest" contemCascaTest,TestLabel "naoContemCascaTest" naoContemCascaTest,TestLabel "naoContemBatataTest" naoContemBatataTest]

--Test for search

procuraBatataTest = TestCase (assertEqual "Procura batata," 0 (search "batata" listaUmElemento))

procuraCascaTest = TestCase (assertEqual "Procura Casca," 2 (search "casca" listaDoisElementos))

naoAchaBatataTest = TestCase (assertEqual "Procura batata," (-1) (search "batata" listaSemBatata))

naoAchaCascaTest = TestCase (assertEqual "Procura Casca," (-1) (search "casca" listaSemCasca))

testsSearch = TestList [TestLabel "procuraBatataTest" procuraBatataTest, TestLabel "procuraCascaTest" procuraCascaTest,TestLabel "naoAchaBatataTest" naoAchaBatataTest,TestLabel "naoAchaCascaTest" naoAchaCascaTest]


--Test for hash

hashBatataTest = TestCase (assertEqual "Hash batata," 0 (hash "batata" 0))

hashCascaTest = TestCase (assertEqual "Hash casca," 2 (hash "casca" 0))

testsHash = TestList [TestLabel "hashBatataTest" hashBatataTest, TestLabel "hashCascaTest" hashCascaTest]

--Test for CurrentSize

tamanhoUmElementoTest = TestCase (assertEqual "Um elemnto," 1 (currentSize listaUmElemento))

tamanhoDoisElementosTest = TestCase (assertEqual "Dois elementos," 2 (currentSize listaDoisElementos))

tamanhoSemUmElementoTest = TestCase (assertEqual "Um elemento," 1 (currentSize listaSemBatata))

tamanhoSemDoisElementosTest = TestCase (assertEqual "Vazia," 0 (currentSize listaSemCasca))

testsCurrentSize = TestList [TestLabel "tamanhoUmElementoTest" tamanhoUmElementoTest, TestLabel "tamanhoDoisElementosTest" tamanhoDoisElementosTest, TestLabel "tamanhoSemUmElementoTest" tamanhoSemUmElementoTest, TestLabel "tamanhoSemDoisElementosTest" tamanhoSemDoisElementosTest]

--Test for isFull

listaUmElementoCheiaTest = TestCase (assertBool"Nao esta cheia" (not(isFull listaUmElemento)))

listaDoisElementosCheiaTest = TestCase (assertBool "Nao esta cheia" (not(isFull listaDoisElementos)))

listaTresElementos = insert "coco" listaDoisElementos

listaTresElementosCheiaTest = TestCase (assertBool "Esta Cheia," (isFull listaTresElementos))

testsIsFull = TestList [TestLabel "listaUmElementoCheiaTest" listaUmElementoCheiaTest, TestLabel "listaDoisElementosCheiaTest" listaDoisElementosCheiaTest, TestLabel "listaTresElementosCheiaTest" listaTresElementosCheiaTest]
