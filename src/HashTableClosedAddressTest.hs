{-
	Test for Hash table closed address
-}

import HashTableClosedAddress
import Test.HUnit
import qualified Data.Map as M

--Tests for insert

listaVazia = M.empty

listaVaziaTest = TestCase (assertEqual "Lista vazia," 0 (M.size listaVazia))

listaUmElemento = insert "batata" listaVazia

listaUmElementoTest = TestCase (assertEqual "Lista com 1 elemento," (M.fromList [(6,["batata"])]) listaUmElemento)

listaDoisElementos = insert "casca" listaUmElemento

listaDoisElementosTest = TestCase (assertEqual "Lista com 2 elementos," (M.fromList [(5,["casca"]),(6,["batata"])]) listaDoisElementos) 

testsInsert = TestList [TestLabel "listaVaziaTest" listaVaziaTest, TestLabel "listaUmElementoTest" listaUmElementoTest, TestLabel "listaDoisElementosTest" listaDoisElementosTest]

--Tests for remove


tamanhoLista = TestCase (assertEqual "Size" 30 size)

listaSemBatata = remove "batata" listaDoisElementos

listaSemBatataTest = TestCase (assertEqual "Remove batata," (M.fromList [(5,["casca"]),(6,[])]) listaSemBatata)

listaSemCasca = remove "casca" listaSemBatata

listaSemCascaTest = TestCase (assertEqual "Remove casca," (M.fromList [(5,[]),(6,[])]) listaSemCasca)

testsRemove = TestList [TestLabel "tamanhoLista" tamanhoLista, TestLabel "listaSemBatataTest" listaSemBatataTest, TestLabel "listaSemCascaTest" listaSemCascaTest ]

--Test for contain

listaContemBatataTest = TestCase (assertBool"Contém batata," (contain "batata" listaUmElemento))

listaContemCascaTest = TestCase (assertBool "Contém casca," (contain "casca" listaDoisElementos))

listaNaoContemBatataTest = TestCase (assertBool "Nao contém batata," (not(contain "batata" listaSemBatata)))

listaNaoContemCascaTest = TestCase (assertBool "Nao contém casca," (not(contain "casca" listaSemCasca)))

testsContain = TestList [TestLabel "listaContemBatataTest" listaContemBatataTest,TestLabel "listaContemCascaTest" listaContemCascaTest,TestLabel "listaNaoContemBatataTest" listaNaoContemBatataTest,TestLabel "listaNaoContemCascaTest" listaNaoContemCascaTest]

--Test for search

procuraBatataTest = TestCase (assertEqual "Procura batata," 6 (search "batata" listaUmElemento))

procuraCascaTest = TestCase (assertEqual "Procura Casca," 5 (search "casca" listaDoisElementos))

naoAchaBatataTest = TestCase (assertEqual "Procura batata," (-1) (search "batata" listaSemBatata))

naoAchaCascaTest = TestCase (assertEqual "Procura Casca," (-1) (search "casca" listaSemCasca))


testsSearch = TestList [TestLabel "procuraBatataTest" procuraBatataTest, TestLabel "procuraCascaTest" procuraCascaTest,TestLabel "naoAchaBatataTest" naoAchaBatataTest,TestLabel "naoAchaCascaTest" naoAchaCascaTest]

--Test for hash

hashBatataTest = TestCase (assertEqual "Hash batata," 6 (hash "batata"))

hashCascaTest = TestCase (assertEqual "Hash casca," 5 (hash "casca"))

testsHash = TestList [TestLabel "hashBatataTest" hashBatataTest, TestLabel "hashCascaTest" hashCascaTest]




 



