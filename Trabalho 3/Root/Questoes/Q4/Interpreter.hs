module Root.Questoes.Q4.Interpreter where

import Root.Questoes.Q4.AbsLI
import Prelude hiding (lookup)

type RContext = [(String, Integer)]
type ErrorMessage = String

{- Dica: somente o tipo de executeP precisa mudar (conforme abaixo),
   mas a sua definicao (corpo) pode continuar a mesma dos exercícios anteriores
-}
executeP :: RContext -> Program -> Either RContext RContext
-- executeP context (Prog stm) = execute context stm
executeP context (Prog stm) = execute context stm

{- Dica: o tipo de execute deve mudar para
 execute :: RContext -> Stm -> Either ErrorMessage RContext
 Alem disso, o corpo dessa funcao deve ser modificado, pois eval
 retorna tipo diferente agora, ou seja, a avaliacao pode falhar
 e, consequentemente o execute tambem. Assim, todos tipos de comandos
 serao afetados
-}
execute :: RContext -> Stm -> Either RContext RContext
execute context x = case x of
  SAss id exp -> case (eval context exp) of 
                    Right ve1 -> Right(update context (getStr id) ve1)
                    Left err -> Left context
                    
  SBlock [] -> Right(context)
  SBlock (s : stms) -> case execute context s of 
                        Right ve1 -> execute ve1 (SBlock stms)
                        Left err -> Left context
  SWhile exp stm -> case eval context exp of 
                      Right ve1 -> if ((ve1) /= 0) then case (execute context stm) of
                                  Right ve11 -> execute ve11 (SWhile exp stm)
                                  Left err -> Left context
                               else Right context
                      Left err -> Left context

  STry stm1 stm2 stm3 -> case execute context (SBlock stm1) of
                            Right ve1 -> execute ve1 (SBlock stm3) 
                            Left err -> case execute err (SBlock stm2) of
                                            Right ve11 -> execute ve11 (SBlock stm3) 
                                            Left err -> execute err (SBlock stm3)


{- Dica: o tipo de eval deve mudar para
 eval :: RContext -> Exp -> Either ErrorMessage Integer
-}
eval :: RContext -> Exp -> Either ErrorMessage Integer
eval context x = case x of
  EAdd exp0 exp -> case (eval context exp0) of 
                   Right ve1 -> case (eval context exp) of 
                                  Right ve11 -> Right(ve1 + ve11) 
                                  Left err -> Left err 
                   Left err -> Left err

  ESub exp0 exp -> case (eval context exp0) of 
                   Right ve1 -> case (eval context exp) of 
                                  Right ve11 -> Right(ve1 - ve11) 
                                  Left err -> Left err 
                   Left err -> Left err

  EMul exp0 exp -> case (eval context exp0) of 
                   Right ve1 -> case (eval context exp) of 
                                  Right ve11 -> Right(ve1 * ve11) 
                                  Left err -> Left err 
                   Left err -> Left err
                
  EVar id -> Right(lookup context (getStr id))
                

  EInt n  ->  Right n
  EDiv e1 e2 -> case (eval context e1) of
                  Right ve1 -> case (eval context e2) of
                                  Right ve2 -> if (ve2 == 0)
                                                then Left "divisao por 0"
                                                else Right (ve1 `div` ve2)
                                  Left msg -> Left msg
                  Left msg -> Left msg
  


-- Dica: voce nao precisa mudar o codigo a partir daqui

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Integer
lookup ((i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s

update :: RContext -> String -> Integer -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv
