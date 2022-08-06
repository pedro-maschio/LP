module Root.Exercicios.UnBCare where

import Root.Modelo.ModeloDados
import Data.List (sortBy)

{-
 *** Aluno: Pedro de Torres Maschio
 *** Matricula: 190018763
 

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
O modelo de dados do problema (definições de tipo) está disponível no arquivo Modelo/ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}


existeMedicamento :: Medicamento -> EstoqueMedicamentos -> Bool 

existeMedicamento _ [] = False 
existeMedicamento medicamento ((med, qtd):es)
                           | medicamento == med = True 
                           | otherwise = existeMedicamento medicamento es 

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med qtd [] = [(med, qtd)]
comprarMedicamento med qtd ((me, qt):es) 
                        |  not(existeMedicamento med ((me, qt):es)) = (med, qtd) : ((me, qt):es)
                        |  med == me = (med, qtd + qt) : es 
                        |  otherwise = (me, qt):comprarMedicamento med qtd es
{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}


tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento _ [] = Nothing 
tomarMedicamento m ((med, qtd):es)
                        | not(existeMedicamento m ((med, qtd):es)) = Nothing 
                        | m == med =  Just((med, qtd-1):es)
                        | otherwise = Just((med, qtd):realValue(tomarMedicamento m es))
                  where realValue (Just x) = x


{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}


consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento _ [] = 0
consultarMedicamento m ((med, qtd):es)
         | m == med = qtd + (consultarMedicamento med es) 
         | otherwise = (consultarMedicamento m es)

{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
-- demandaMedicamentos ((med, hours):recs) = sortBy (\(a,_) (b,_) -> compare a b) ((med, length hours):(demandaMedicamentos recs))
demandaMedicamentos ((med, hours):recs) = ((med, length hours):(demandaMedicamentos recs))

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}


-- Retorna True se os planos/receituários estão ordenados (se estão estritamente ordenados, necessariamente são distintos/unicos)
estaoOrdenados :: (Ord recplan) => [recplan] -> Bool 
estaoOrdenados [] = True 
estaoOrdenados [x] = True
estaoOrdenados (x:y:z) = (x < y) && estaoOrdenados (y:z)


receituarioValido :: Receituario -> Bool
receituarioValido recs
      | estaoOrdenados recs && (length (filter estaoOrdenados [horarios | (_, horarios) <- recs]) == length recs)  = True
      | otherwise = False

-- type PlanoMedicamento = [(Horario, [Medicamento])]
planoValido :: PlanoMedicamento -> Bool
planoValido planos 
   | estaoOrdenados planos && (length (filter estaoOrdenados [meds | (_, meds) <- planos]) == length planos) = True 
   | otherwise = False

{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}


-- Lista os medicamentos de Medicar
listarMedicamentosMedicar :: [Cuidado] -> [Medicamento]
listarMedicamentosMedicar [] = []
listarMedicamentosMedicar ((Comprar med qtd):cuidados) = listarMedicamentosMedicar cuidados
listarMedicamentosMedicar ((Medicar med):cuidados) = med:(listarMedicamentosMedicar cuidados)

-- Verifica as ocorrências de Comprar
checkCompra :: Medicamento -> [Cuidado] -> Bool 
checkCompra _ [] = True 
checkCompra m ((Comprar med qtd):cuidados) = checkCompra m cuidados 
checkCompra m ((Medicar med):cuidados)
   | m == med = False
   | otherwise =  checkCompra m cuidados 

-- Verifica as ocorrências de Medicar
checkMedicar :: Medicamento -> [Cuidado] -> Bool 
checkMedicar _ [] = True 
checkMedicar m ((Medicar med):cuidados) = checkMedicar m cuidados 
checkMedicar m ((Comprar med qtd):cuidados)
   | m == med = False
   | otherwise =  checkCompra m cuidados 

-- Retorna true se não houver ocorrência de compra e medicagem do mesmo medicamento
checkCuidados :: [Cuidado] -> Bool 
checkCuidados [] = True 
checkCuidados ((Comprar med qtd):cuidados) = (checkCompra med cuidados) && (checkCuidados cuidados)
checkCuidados ((Medicar med):cuidados) = (checkMedicar med cuidados) && (checkCuidados cuidados)

plantaoValido :: Plantao -> Bool
plantaoValido plantoes 
   | estaoOrdenados [horario | (horario, _) <- plantoes] && length(filter checkCuidados [cuidados | (h, cuidados) <- plantoes]) == (length [cuidados | (h, cuidados) <- plantoes]) && length(filter estaoOrdenados (map listarMedicamentosMedicar [cuidados | (h, cuidados) <- plantoes])) == length(map listarMedicamentosMedicar [cuidados | (h, cuidados) <- plantoes]) = True
   | otherwise = False

{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

-- Ordena os horários e remove as repetições
ordena [] = []
ordena (x:resto) = ordena [y | y <- resto, y < x]++[x]++ordena [y | y <- resto, y > x]

-- Transforma uma única prescrição em um plano de medicamento parcial
prescricaoEmPlano :: Prescricao -> PlanoMedicamento
prescricaoEmPlano (_, []) = []
prescricaoEmPlano (med, h1:hors) = (h1, [med]):(prescricaoEmPlano (med, hors))

-- Pega todos os horários do Receituario
pegaHorarios :: Receituario -> [Horario]
pegaHorarios [] = []
pegaHorarios ((med, hors):recs) = [h | h <- hors] ++ pegaHorarios recs

-- Dada a lista de horários, monta o plano de medicamento corretamente
montaPlano :: [Horario] -> PlanoMedicamento -> PlanoMedicamento
montaPlano [] [] = []
montaPlano h1 [] = []
montaPlano [] h2 = []
montaPlano (h1:restoHorarios) ((h, m:meds):restoPlano) = (h1, [me | (hor, me:ms) <- ((h, m:meds):restoPlano), hor == h1]):(montaPlano restoHorarios restoPlano)

-- Apenas concatena o resultado de prescricaoEmPlano
parsingPlano :: Receituario -> PlanoMedicamento
parsingPlano [] = []
parsingPlano (p1:recs) = (prescricaoEmPlano p1)++(parsingPlano recs)

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario [] = []
geraPlanoReceituario (p1:recs) = montaPlano (ordena(pegaHorarios (p1:recs))) (ordena(parsingPlano (p1:recs)))


{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}


-- Pega todos os medicamentos do PlanoMedicamento
pegaMedicamentos :: PlanoMedicamento -> [Medicamento]
pegaMedicamentos [] = []
pegaMedicamentos ((hor, meds):recs) = [med | med <- meds] ++ pegaMedicamentos recs

-- Transforma um par (Horario, [Medicamento]) em um receituário parcial
parEmReceituario :: (Horario, [Medicamento]) -> Receituario
parEmReceituario (_, []) = []
parEmReceituario (hor, m1:meds) = (m1, [hor]):(parEmReceituario (hor, meds))

-- Dada a lista de medicamentos, monta o receituário corretamente
montaReceituario :: [Medicamento] -> Receituario -> Receituario
montaReceituario [] [] = []
montaReceituario h1 [] = []
montaReceituario [] h2 = []
montaReceituario (m1:restoMedicamentos) ((med, h:hs):restoReceituario) = (m1, [ho | (me, ho:hours) <- ((med, h:hs):restoReceituario), me == m1]):(montaReceituario restoMedicamentos restoReceituario)

-- Apenas concatena o resultado de parEmReceituario
parsingReceituario :: PlanoMedicamento -> Receituario
parsingReceituario [] = []
parsingReceituario (p1:recs) = (parEmReceituario p1)++(parsingReceituario recs)

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano [] = []
geraReceituarioPlano (p1:planos) = montaReceituario (ordena(pegaMedicamentos (p1:planos))) (ordena (parsingReceituario(p1:planos)))

{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

-- Diminui em um a quantidade de um medicamento no estoque
atualizaEstoque :: Medicamento -> EstoqueMedicamentos -> EstoqueMedicamentos
atualizaEstoque _ [] = []
atualizaEstoque med ((me, qt):es)
   | med == me = (me, qt-1):es 
   | otherwise = (me, qt):(atualizaEstoque med es)

-- Verifica se é possível fornecer esses cuidados dado o Estoque de Medicamentos 
ehPossivel :: [Cuidado] -> EstoqueMedicamentos -> Bool 
ehPossivel [] _ = True 
ehPossivel ((Comprar med qtd):cuidados) estoque = ehPossivel cuidados (comprarMedicamento med qtd estoque)
ehPossivel ((Medicar med):cuidados) estoque
   | consultarMedicamento med estoque == 0 = False 
   | otherwise = ehPossivel cuidados estoque


-- Realiza os cuidados medicar e comprar, em ambos os casos atualiza o estoque
realizaCuidados :: [Cuidado] -> EstoqueMedicamentos -> EstoqueMedicamentos
realizaCuidados [] estoque = estoque 
realizaCuidados ((Comprar med qtd):cuidados) estoque = realizaCuidados cuidados (comprarMedicamento med qtd estoque)
realizaCuidados ((Medicar med):cuidados) estoque = realizaCuidados cuidados (atualizaEstoque med estoque)
   
executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao [] estoque = Just estoque 
executaPlantao ((_, cuidados):plantoes) estoque
   | not(ehPossivel cuidados estoque) = Nothing 
   | otherwise = executaPlantao plantoes (realizaCuidados cuidados estoque)


{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

-- Dado um horário e lista de cuidados, retorna uma lista de horarios e medicamentos a partir desses cuidados
obtemParPlano :: Horario -> [Cuidado] -> [(Horario, Medicamento)]
obtemParPlano _ [] = []
obtemParPlano h ((Medicar med):cuidados) = (h, med):(obtemParPlano h cuidados)
obtemParPlano h ((Comprar med qtd):cuidados) = obtemParPlano h cuidados

-- Dado um plantão, retorna uma lista de horários e medicamentos
parsingPlantao :: Plantao -> [(Horario, Medicamento)]
parsingPlantao [] = []
parsingPlantao ((hor, cuidados):plantoes) = (obtemParPlano hor cuidados) ++ (parsingPlantao plantoes)

-- Dado um  horário e uma lista de medicamentos, retorna uma lista de pares com o mesmo horário e medicamentos
obtemParMeds :: Horario -> [Medicamento] -> [(Horario, Medicamento)]
obtemParMeds _ [] = []
obtemParMeds h (med:meds) = (h, med):(obtemParMeds h meds)

-- Dado um plano, retorna uma lista de horários e medicamentos
parsingPlanoTupla :: PlanoMedicamento -> [(Horario, Medicamento)]
parsingPlanoTupla [] = []
parsingPlanoTupla ((hor, meds):planos) = (obtemParMeds hor meds)++(parsingPlanoTupla planos)

-- Verificamos se os plantoes correspondem com os planos, podemos usar o operador de igualdade pois
-- a igualdade está definida para as listas de pares (Int, string)
ehIgual :: Plantao -> PlanoMedicamento -> Bool
ehIgual plantao plano = (parsingPlantao plantao) == (parsingPlanoTupla plano)

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz _ [] _ = False 
satisfaz plantao plano estoque = (executaPlantao plantao estoque /= Nothing) && (ehIgual plantao plano) 

{-

QUESTÃO 11 VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

-- type PlanoMedicamento = [(Horario, [Medicamento])]
-- type EstoqueMedicamentos = [(Medicamento, Quantidade)]
-- type Plantao = [(Horario, [Cuidado])]
-- data Cuidado = Comprar Medicamento Quantidade | Medicar Medicamento


-- consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
-- Se não houver estoque, compramos uma únidade do medicamento, se tiver, medicamos uma unidade também. Dessa forma criamos
-- uma lista de cuidados que faz sentido e no final geramos um plantão correto
constroiPlantao :: [Medicamento] -> EstoqueMedicamentos -> [Cuidado]
constroiPlantao [] estoque = []
constroiPlantao (med:meds) estoque 
   | (consultarMedicamento med estoque) <= 0 = (Comprar med 1):(constroiPlantao meds estoque)
   | (consultarMedicamento med estoque) > 0 = (Medicar med):(constroiPlantao meds estoque)


plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto [] estoque = []
plantaoCorreto ((hor, meds):planos) estoque = (hor, constroiPlantao meds estoque):(plantaoCorreto planos estoque)
