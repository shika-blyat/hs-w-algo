module Main where

import Prelude hiding (lookup)

import Data.Map (Map, lookup, empty)

data Expr = EVar String | ELit Literal | ECall Expr Expr | EFun String Expr

newtype Literal = Number Int deriving Show

data Type = TNamed String | TVar String | TFun Type Type deriving Show

type Context = Map String Type
type Subst = Map String Type

infer :: Expr -> Context -> Either (Type, Subst) String

infer (ELit lit) _ = Left (TNamed $ show lit, empty)

infer (EVar name) ctx = case Data.Map.lookup name ctx of
    Just val -> Left (val, empty)
    Nothing -> Right ("Use of undeclared variable" ++ show name)

--infer (ECall from to) ctx = case Data.Map.lookup name ctx of
    -- Just val -> Left val
    -- Nothing -> Right ("Use of undeclared variable" ++ show name)

main :: IO ()

main = print $ infer (ELit (Number 15)) empty
