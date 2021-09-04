module UnBCare where

import ModeloDados

{-

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝



O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

-- Aluno: Carlos Gabriel Vilas Novas Soares
-- Matrícula: 180056298
{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

-- comprarMedicamento medicamento quantidade estoqueMedicamento 
--    |  estoqueMedicamento /= [] = [(medicamento, quantidade+estoqueExistente)]
--    |  otherwise           = [(medicamento, quantidade)]
--    where (_,estoqueExistente) = head estoqueMedicamento

-- Retorna lista vazia caso o medicamento nao exista
buscaNaLista :: Medicamento -> EstoqueMedicamentos -> EstoqueMedicamentos
buscaNaLista medicamento estoque = [tupla | tupla <- estoque, fst tupla == medicamento]

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento medicamento quantidade [] = [(medicamento, quantidade)]
comprarMedicamento medicamento quantidade ((medEst,qtdEst):xs)
   | null (buscaNaLista medicamento ((medEst,qtdEst):xs)) = (medicamento, quantidade):((medEst,qtdEst):xs)
   | medicamento == medEst = (medicamento, quantidade+qtdEst):xs
   | otherwise =  (medEst,qtdEst) : comprarMedicamento medicamento quantidade xs

{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

buscaListaBool :: Medicamento -> EstoqueMedicamentos -> Bool
buscaListaBool _ [] = False
buscaListaBool medicamento  (x:xs)
   |  medicamento == fst x = True
   |  otherwise  = buscaListaBool medicamento xs

medicamentoDisponivel :: Medicamento -> EstoqueMedicamentos  -> EstoqueMedicamentos
medicamentoDisponivel medicamento estoque = [tupla | tupla <- estoque, fst tupla == medicamento && snd tupla >= 1]

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento [] [] = Nothing
tomarMedicamento (_:_) [] = Nothing
tomarMedicamento medicamento (x:xs)
   | buscaListaBool medicamento (x:xs) && not (null (medicamentoDisponivel medicamento (x:xs))) = Just $ map(\x -> if fst x  == medicamento && snd x >=1 then (fst x, snd x -1) else x) (x:xs)
   | otherwise = Nothing

{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}
-- consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
-- consultarMedicamento = undefined
consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento [] [] = 0
consultarMedicamento (_:_) [] = 0
consultarMedicamento medicamento (x:xs)
   |  not (null result) =  snd (head result)
   |  otherwise  = 0
   where result = buscaNaLista medicamento (x:xs)


{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

-- remover o argumento "receituario" foi sugestão da extensão de haskell, achei roubado
demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos  = map (\ (med, horarios) -> (med, length horarios))


{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}
todosDiferentes :: (Eq a) => [a] -> Bool
todosDiferentes [] = True
todosDiferentes (x:xs) = x `notElem` xs && todosDiferentes xs

ordenado :: (Ord a) => [a] -> Bool
ordenado [] = True
ordenado [x] = True
ordenado (x:y:xs) = x < y && ordenado (y:xs)

receituarioValido :: Receituario -> Bool
receituarioValido receituario
   |  ordenado med && all ordenado horarios = True
   |  otherwise  = False
   where med = map fst receituario
         horarios = map snd receituario

planoValido :: PlanoMedicamento -> Bool
planoValido plano
   | all ordenado medicamentos && ordenado horario = True
   | otherwise = False
   where horario = map fst plano
         medicamentos = map snd plano


{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}


--  Cumpre a primeira condição ou não, semelhante ao planoValido
plantaoHorarioValido :: Plantao -> Bool
plantaoHorarioValido plantao
   | ordenado horario = True
   | otherwise = False
   where horario = map fst plantao

-- Segunda condicao
boolList tupla
   |  fst tupla == snd tupla = True
   |  otherwise  = False

checkBoolLista xs = if all (== False) xs then True else False

plantaoComprarMedicarValido :: Plantao -> Bool
plantaoComprarMedicarValido plantao = result
   where item = map snd plantao
         -- Separamos medicar / comprar
         medicarLista = map (filter medicarConst ) item
         comprarLista = map (filter comprarConst) item
         -- Construtores para podermos filtrar somente o medicar/comprar
         medicarConst (Medicar _) = True
         medicarConst _ = False
         comprarConst (Comprar _ _) = True
         comprarConst _ = False
         -- Nós limpamos o elemento da lista para ficar somente o nome de cada remedio
         medicarLimpo = map (map (\(Medicar m) -> m)) medicarLista
         comprarLimpo = map (map (\(Comprar m _) -> m)) comprarLista
         -- Juntamos ambas as listas
         zipado = zip medicarLimpo comprarLimpo
         -- Criamos um vetor para verificar se os elementos existem em comprar e medicar
         booleanListResult = map boolList zipado
         -- Verificamos se todos são false (nao existem no mesmo horario)
         result = checkBoolLista booleanListResult

-- Funcao principal
-- O caso plantaoInvalido4 FALHA pois ele nao compara todos os elementos com todos, infelizmente não consegui fazer isso
plantaoValido :: Plantao -> Bool
plantaoValido plantao
   |  plantaoHorarioValido plantao && plantaoComprarMedicarValido plantao = True
   |  otherwise = False


{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

expandeLista :: Prescricao -> [(Medicamento, Horario )]
expandeLista ([], []) = []
expandeLista ((_:_), []) = []
expandeLista (a, x:xs) = (a, x):expandeLista (a, xs)

-- Três passos: 
-- Gerar array para cada medicamento de cara hora
-- Reagrupar baseando no horario

-- Implementando uma funcao para agrupar
-- https://stackoverflow.com/questions/45654216/haskell-groupby-function-how-exactly-does-it-work
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _  []     =  []
groupBy eq (x:xs) =  (x:ys) : groupBy eq zs
    where (ys,zs) = span (eq x) xs

inverte :: (a,b) -> (b,a)
inverte (a,b) = (b,a)

-- quicksort
-- https://smthngsmwhr.wordpress.com/2012/11/09/sorting-algorithms-in-haskell/
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

pegaHorarios :: [(a, b)] -> [b]
pegaHorarios lista = resultado
   where resultado =  map snd lista
      

-- pegaHora' lista = 
pegaMedicamentos :: [(b1, b2)] -> [b1]
pegaMedicamentos lista = map fst lista


-- https://stackoverflow.com/questions/8227218/removing-repeated-elements-from-a-list-in-haskell
removeDuplicata :: (Ord a, Eq a) => [a] -> [a]
removeDuplicata xs = remove $ quicksort xs
  where
    remove []  = []
    remove [x] = [x]
    remove (x1:x2:xs)
      | x1 == x2  = remove (x1:xs)
      | otherwise = x1 : remove (x2:xs)
      
geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario [] = []
geraPlanoReceituario receituario = resultado
   where expandida = map expandeLista receituario
         flat = concat expandida
         -- Invertemos a tupla
         flatInv = map inverte flat
         -- Fazemos um sort usando o primeiro elemento da tupla
         sortedFlatInv = quicksort flatInv
         -- Invertemos novamente para a posicao normal para poder agrupar
         sortedFlat = map inverte sortedFlatInv
         -- Agrupamos os elementos 
         -- Em seguida agrupamos os elementos
         groupedFlat = groupBy (\a b -> snd a == snd b) sortedFlat
         -- Separamos os medicamentos
         listaMedicamentos = map pegaMedicamentos groupedFlat 
         -- Separamos os horarios
         listaHorarios = map pegaHorarios groupedFlat
         -- Concatenamos eles
         concatHorarios = concat listaHorarios
         -- limpamos de duplicatas
         uniqueList = removeDuplicata concatHorarios
         -- Temos já a certeza que as listas seram do mesmo tamanho, logo:
         resultado = zip uniqueList listaMedicamentos



{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}
-- Entendi a relação entre a questão 7 e 8, entretando não consegui demonstrar
-- Essa relação através do código, a questão foi basicamente control c + control v

expandeListaPlano :: (Horario,[Medicamento]) -> [(Horario, Medicamento)]
expandeListaPlano (_, []) = []
expandeListaPlano (a, x:xs) = (a, x) : expandeListaPlano (a, xs)

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano planoMedicamento = resultado
   where expandida = map expandeListaPlano planoMedicamento
         flat = concat expandida
         -- -- Invertemos a tupla
         flatInv = map inverte flat
         -- -- Fazemos um sort usando o primeiro elemento da tupla
         sortedFlatInv = quicksort flatInv
         -- -- Invertemos novamente para a posicao normal para poder agrupar
         sortedFlat = map inverte sortedFlatInv
         -- -- Agrupamos os elementos 
         -- -- Em seguida agrupamos os elementos
         groupedFlat = groupBy (\a b -> snd a == snd b) sortedFlat
         -- -- Separamos os medicamentos
         listaMedicamentos = map pegaMedicamentos groupedFlat 
         -- -- Separamos os horarios
         listaHorarios = map pegaHorarios groupedFlat
         -- -- Concatenamos eles
         concatHorarios = concat listaHorarios
         -- -- limpamos de duplicatas
         uniqueList = removeDuplicata concatHorarios
         -- -- Temos já a certeza que as listas seram do mesmo tamanho, logo:
         resultado = zip uniqueList listaMedicamentos
  

{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado 
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao = undefined


{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano 
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão 
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos  -> Bool
satisfaz = undefined


{-

QUESTÃO 11 (EXTRA) VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

plantaoCorreto :: PlanoMedicamento ->  EstoqueMedicamentos  -> Plantao
plantaoCorreto = undefined

