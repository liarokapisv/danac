{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}


module Danac.Renamer where

import Danac.Ast
import Danac.Util.Annotation
import Danac.Util.SourceSpan
import Data.Text (Text)
import Validation
import Control.Monad.Reader
import Data.Comp.Multi.Ops (ffst, fsnd, (:*:)(..))
import Data.Comp.Multi.Algebra
import Data.Comp.Multi.Term (Term)
import Data.Comp.Multi.HTraversable (htraverse)
import Data.Functor.Compose
import Data.List (elemIndex)

data Error = UndefinedVariable Text SourceSpan
           | AlreadyDefined Text SourceSpan
    deriving Show

data Frame = Frame {
    functionName :: Text,
    variables :: [Text]
}

type Context = [Frame]


lookVariable :: Text -> Context -> Maybe (Text, Int, Int)
lookVariable t c = go 0 c
    where go _ [] = Nothing
          go n (Frame {functionName = fn, variables = vs} : fs) = 
                case elemIndex t (reverse vs) of
                        Just i -> Just (fn, n, i)
                        Nothing -> go (n+1) fs

data NoAnnGroup i where
    NoAnnAst :: NoAnnGroup Ast
    NoAnnFuncDef :: NoAnnGroup FuncDef
    NoAnnHeader :: NoAnnGroup Header
    NoAnnFparDef :: NoAnnGroup FparDef
    NoAnnDataType :: NoAnnGroup DataType
    NoAnnObjectType :: NoAnnGroup ObjectType
    NoAnnType :: NoAnnGroup Type
    NoAnnParPassType :: NoAnnGroup ParPassType
    NoAnnLocalDef :: NoAnnGroup LocalDef
    NoAnnFuncDecl :: NoAnnGroup FuncDecl
    NoAnnVarDef :: NoAnnGroup VarDef
    NoAnnCondStmt :: NoAnnGroup CondStmt
    NoAnnStmt :: NoAnnGroup Stmt
    NoAnnBlock :: NoAnnGroup Block
    NoAnnFuncCall :: NoAnnGroup FuncCall
    NoAnnLvalue :: NoAnnGroup Lvalue
    NoAnnExpr :: NoAnnGroup Expr
    NoAnnCond :: NoAnnGroup Cond

deriving instance (Show (NoAnnGroup i))

data Ann i where
    AnnVariable :: SourceSpan -> (Text, Int, Int) -> Ann Variable
    NoAnn :: SourceSpan -> NoAnnGroup i -> Ann i

deriving instance (Show (Ann i))

collectHeaderVarNames :: Term (T :&: SourceSpan) Header -> Either Error (Text, [Text])
collectHeaderVarNames (Header name _ fdefs :&.: _) = fmap (\s -> (name, reverse s )) $ go [] fdefs
    where go :: [Text] -> [Term (T :&: SourceSpan) FparDef] -> Either Error [Text]
          go n [] = Right n
          go n (FparDef t _ :&.: s : fs) = 
                    if t `elem` n 
                        then Left $ AlreadyDefined t s
                        else go (t : n) fs


collectLocalVarNames :: [Text] -> [Term (T :&: SourceSpan) LocalDef] -> Either Error [Text]
collectLocalVarNames n [] = Right n
collectLocalVarNames n (LocalDefFuncDef _ :&.: _ : ds) = collectLocalVarNames n ds
collectLocalVarNames n (LocalDefFuncDecl _ :&.: _ : ds) = collectLocalVarNames n ds
collectLocalVarNames n (LocalDefVarDef (VarDef t _ :&.: s) :&.: _ : ds) = 
                    if t `elem` n 
                        then Left $ AlreadyDefined t s
                        else collectLocalVarNames (t : n) ds

createFrame :: Term (T :&: SourceSpan) Header -> [Term (T :&: SourceSpan) LocalDef] -> Either Error Frame
createFrame header ldefs = do
    (name, names1) <- collectHeaderVarNames header
    names2 <- collectLocalVarNames names1 ldefs
    pure $ Frame { functionName = name, variables = names1 ++ names2 }

data View r i where
    VariableView :: T r Variable -> View r Variable
    NoAnnView :: T r i -> NoAnnGroup i -> View r i

view :: T r i -> View r i
view y = case group y of
    GroupVariable x -> VariableView x
    GroupAst x -> NoAnnView x NoAnnAst
    GroupFuncDef x -> NoAnnView x NoAnnFuncDef
    GroupHeader x -> NoAnnView x NoAnnHeader
    GroupFparDef x -> NoAnnView x NoAnnFparDef
    GroupDataType x -> NoAnnView x NoAnnDataType
    GroupObjectType x -> NoAnnView x NoAnnObjectType
    GroupType x -> NoAnnView x NoAnnType
    GroupParPassType x -> NoAnnView x NoAnnParPassType
    GroupLocalDef x -> NoAnnView x NoAnnLocalDef
    GroupFuncDecl x -> NoAnnView x NoAnnFuncDecl
    GroupVarDef x -> NoAnnView x NoAnnVarDef
    GroupCondStmt x -> NoAnnView x NoAnnCondStmt
    GroupStmt x -> NoAnnView x NoAnnStmt
    GroupBlock x -> NoAnnView x NoAnnBlock
    GroupFuncCall x -> NoAnnView x NoAnnFuncCall
    GroupLvalue x -> NoAnnView x NoAnnLvalue
    GroupExpr x -> NoAnnView x NoAnnExpr
    GroupCond x -> NoAnnView x NoAnnCond

newtype Renamer f i = Renamer { getRenamer :: Compose (Reader Context) (Validation [Error]) (f i) }

renameAlg :: RAlg (T :&: SourceSpan) (Renamer (Term (T :&&: Ann))) 
renameAlg (t :&: s) = 
    case view t of
        VariableView (Variable name) -> Renamer $ Compose $ do
            minfo <- asks $ lookVariable name
            case minfo of
                Nothing -> pure $ Failure $ [UndefinedVariable name s]
                Just info -> pure $ Success $ Variable name :&&.: AnnVariable s info
        NoAnnView f@(FuncDef (header :*: _) ldefs _) p -> Renamer $ Compose $ do
            let mframe = createFrame header (fmap ffst ldefs)
            case mframe of
                Left e -> pure $ Failure [e]
                Right frame -> local (frame:) $ 
                    getCompose $ fmap (:&&.: NoAnn s p) $ htraverse (getRenamer . fsnd) f
        NoAnnView x p -> Renamer $ fmap (:&&.: NoAnn s p) $ htraverse (getRenamer . fsnd) x

rename = getRenamer . para renameAlg
