module Root.Questoes.Q5.Interpreter where

import Root.Questoes.Q5.AbsLI

-- As definições deste arquivo são as mínimas para compilar os testes.
-- Você deverá completar todo o restante do código.
-- Dica: se você fez os exercícios anteriores, boa parte do código
-- pode ser reutilizado neste exercício.

type RContext = [(String, Valor)]

type ErrorMessage = String

executeP :: RContext -> Program -> Either RContext RContext
executeP context (Prog stm) = execute context stm

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
                      Right ve1 -> if (ve1 /= ValorInt 0) then case (execute context stm) of
                                  Right ve11 -> execute ve11 (SWhile exp stm)
                                  Left err -> Left context
                               else Right context
                      Left err -> Left context

  STry stm1 stm2 stm3 -> case execute context (SBlock stm1) of
                            Right ve1 -> execute ve1 (SBlock stm3) 
                            Left err -> case execute err (SBlock stm2) of
                                            Right ve11 -> execute ve11 (SBlock stm3) 
                                            Left err -> execute err (SBlock stm3)
  SdoWhile stm exp -> case (execute context stm) of 
                        Right ve1 -> execute ve1 (SWhile exp stm)
                        Left err -> Left context

eval :: RContext -> Exp -> Either ErrorMessage Valor
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
                
  EVar id -> Right(sLookup context (getStr id))

  EOr exp0 exp -> case (eval context exp0) of 
                    Right ve1 -> case (eval context exp) of 
                                  Right ve11 -> Right(ValorBool (b ve1 || b ve11))
                                  Left err -> Left err
                    Left err -> Left err

  EOr exp0 exp -> case (eval context exp0) of 
                    Right ve1 -> case (eval context exp) of 
                                  Right ve11 -> Right(ValorBool (b ve1 && b ve11))
                                  Left err -> Left err
                    Left err -> Left err

  -- ENot exp -> ValorBool(not(b (eval context exp)))
  ENot exp -> case (eval context exp) of 
                Right ve1 -> Right(ValorBool(not(b ve1)))
                Left err -> Left err
  ETrue -> Right(ValorBool True)
  EFalse -> Right(ValorBool False)
                

  EInt n  ->  Right(ValorInt n)
  EDiv e1 e2 -> case (eval context e1) of
                  Right ve1 -> case (eval context e2) of
                                  Right ve2 -> if (ve2 == 0)
                                                then Left "divisao por 0"
                                                else Right (ve1 `div` ve2)
                                  Left msg -> Left msg
                  Left msg -> Left msg

data Valor
  = ValorStr String
  | ValorInt Integer
  | ValorBool Bool

instance Eq Valor where
  (ValorInt i1) == (ValorInt i2) = i1 == i2
  (ValorStr s1) == (ValorStr s2) = s1 == s2
  (ValorBool b1) == (ValorBool b2) = b1 == b2

s :: Valor -> String
s (ValorStr str) = str

i :: Valor -> Integer
i (ValorInt vint) = vint

b :: Valor -> Bool
b (ValorBool vbool) = vbool

getStr :: Ident -> String
getStr (Ident s) = s

sLookup :: RContext -> String -> Valor
sLookup ((i, v) : cs) s
  | i == s = v
  | otherwise = sLookup cs s

update :: RContext -> String -> Valor -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv