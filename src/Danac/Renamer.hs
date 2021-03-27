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
{-# LANGUAGE OverloadedStrings #-}


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
import Data.List (elemIndex, intersperse)

data Error = UndefinedVariable Text SourceSpan
           | UndefinedFunction Text SourceSpan
           | UndefinedLabel Text SourceSpan
           | AlreadyDefinedVariable Text SourceSpan
           | AlreadyDefinedFunction Text SourceSpan
           | AlreadyDefinedLabel Text SourceSpan
    deriving Show

data Frame = Frame {
    functionName :: Text,
    variables :: [Text],
    functions :: [Text]
}

data Context = Context {
    frames :: [Frame],
    globals :: [Text],
    labels :: [Text]
}

emptyContext = Context { frames = [], globals = ["readInteger", "writeString", "strlen"], labels=[] }

withNamespace t names = mconcat $ intersperse "." $ reverse $ (t : names)

lookVariable :: Text -> Context -> Maybe (Text, Int, Int)
lookVariable t c = go 0 (frames c)
    where go _ [] = Nothing
          go n (Frame {functionName = name, variables = vs} : fs) = 
                case elemIndex t (reverse vs) of
                        Just i -> Just (withNamespace name (fmap functionName fs), n, i)
                        Nothing -> go (n+1) fs

lookFunction :: Text -> Context -> Maybe Text
lookFunction t c = case go (frames c) of
                        Just n -> Just n
                        Nothing | t `elem` globals c -> Just t
                                | otherwise -> Nothing
    where go [] = Nothing
          go fs@(f : fs') | t `elem` functions f = Just $ withNamespace t (fmap functionName fs)
                          | otherwise = go fs'

lookLabel :: Text -> Context -> Maybe Text
lookLabel t c = case dropWhile (/= t) (labels c) of
                     [] -> Nothing
                     (x:xs) -> Just $ withNamespace x xs
                
collectHeaderVarNames :: Term (T :&: SourceSpan) Header -> Either Error (Text, [Text])
collectHeaderVarNames (Header name _ fdefs :&.: _) = fmap (\s -> (name, reverse s )) $ go [] fdefs
    where go :: [Text] -> [Term (T :&: SourceSpan) FparDef] -> Either Error [Text]
          go n [] = Right n
          go n (FparDef t _ :&.: s : fs) = 
                    if t `elem` n 
                        then Left $ AlreadyDefinedVariable t s
                        else go (t : n) fs


collectLocalNames :: ([Text],[Text]) -> [Term (T :&: SourceSpan) LocalDef] -> Either Error ([Text],[Text])
collectLocalNames (vars,fns) [] = Right (vars, fns)
collectLocalNames (vars,fns) (LocalDefFuncDef (FuncDef (Header t _ _ :&.: s) _ _ :&.: _) :&.: _ : ds) 
    | t `elem` vars = Left $ AlreadyDefinedFunction t s
    | otherwise = collectLocalNames (vars,(t : fns)) ds
collectLocalNames (vars,fns) (LocalDefFuncDecl (FuncDecl (Header t _ _ :&.: s) :&.: _) :&.: _ : ds) 
    | t `elem` vars = Left $ AlreadyDefinedFunction t s
    | otherwise = collectLocalNames (vars,(t : fns)) ds
collectLocalNames (vars,fns) (LocalDefVarDef (VarDef t _ :&.: s) :&.: _ : ds) 
    | t `elem` vars = Left $ AlreadyDefinedVariable t s 
    | otherwise = collectLocalNames ((t : vars),fns) ds

createFrame :: Term (T :&: SourceSpan) Header -> [Term (T :&: SourceSpan) LocalDef] -> Either Error (Text, Frame)
createFrame header ldefs = do
    (name, vars1) <- collectHeaderVarNames header
    (vars2, fns) <- collectLocalNames (vars1,[]) ldefs
    pure $ (name, Frame { functionName = name, variables = vars2, functions = fns })


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
    NoAnnFunction :: NoAnnGroup FuncIdentifier
    NoAnnFuncCall :: NoAnnGroup FuncCall
    NoAnnLvalue :: NoAnnGroup Lvalue
    NoAnnExpr :: NoAnnGroup Expr
    NoAnnCond :: NoAnnGroup Cond

deriving instance (Show (NoAnnGroup i))

data Ann i where
    AnnVariable :: SourceSpan -> (Text, Int, Int) -> Ann VarIdentifier
    NoAnn :: SourceSpan -> NoAnnGroup i -> Ann i

deriving instance (Show (Ann i))

data View r i where
    VariableView :: T r VarIdentifier -> View r VarIdentifier
    NoAnnView :: T r i -> NoAnnGroup i -> View r i

view :: T r i -> View r i
view y = case group y of
    GroupVarIdentifier x -> VariableView x
    GroupFuncIdentifier x -> NoAnnView x NoAnnFunction
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

renameHeader :: Text -> Term (T :&&: Ann) FuncDef -> Term (T :&&: Ann) FuncDef
renameHeader t (FuncDef (Header _ x y :&&.: z) k l :&&.: m) = FuncDef (Header t x y :&&.: z) k l :&&.: m

renameLoopLabel :: Text -> Term (T :&&: Ann) Stmt -> Term (T :&&: Ann) Stmt
renameLoopLabel t (StmtLoop (Just _) xs :&&.: y) = StmtLoop (Just t) xs :&&.: y
renameLoopLabel _ x = x

renameAlg :: RAlg (T :&: SourceSpan) (Renamer (Term (T :&&: Ann))) 
renameAlg (t :&: s) = 
    case view t of
        VariableView (VarIdentifier name) -> Renamer $ Compose $ do
            minfo <- asks $ lookVariable name
            case minfo of
                Nothing -> pure $ Failure [UndefinedVariable name s]
                Just info -> pure $ Success $ VarIdentifier name :&&.: AnnVariable s info
        NoAnnView f@(FuncDef (header :*: _) ldefs _) p -> Renamer $ Compose $ do
            let mframe = createFrame header (fmap ffst ldefs)
            case mframe of
                Left e -> pure $ Failure [e]
                Right (name, frame) -> do
                    name' <- asks $ withNamespace name . fmap functionName . frames
                    local (\r -> r { frames = frame : frames r}) $
                        getCompose $ fmap (renameHeader name') $ defaultCase f p
        NoAnnView (FuncIdentifier name) p -> Renamer $ Compose $ do
            mf <- asks $ lookFunction name 
            case mf of
                Nothing -> pure $ Failure [UndefinedFunction name s]
                Just name' -> pure $ Success $ (FuncIdentifier name' :&&.: NoAnn s p)
        NoAnnView l@(StmtLoop (Just name) _) p -> Renamer $ Compose $ do
            found <- asks $ elem name . labels
            if not found 
                then do
                    name' <- asks $ withNamespace name . labels
                    local (\r -> r { labels = name : labels r }) $ do
                        getCompose $ fmap (renameLoopLabel name') $ defaultCase l p
                else
                    pure $ Failure [AlreadyDefinedLabel name s]
        NoAnnView (StmtBreak (Just name)) p -> Renamer $ Compose $ do
            mlabel <- asks $ lookLabel name
            case mlabel of
                Nothing -> pure $ Failure [UndefinedLabel name s]
                Just name' -> pure $ Success $ StmtBreak (Just name') :&&.: NoAnn s p
        NoAnnView (StmtContinue (Just name)) p -> Renamer $ Compose $ do
            mlabel <- asks $ lookLabel name
            case mlabel of
                Nothing -> pure $ Failure [UndefinedLabel name s]
                Just name' -> pure $ Success $ StmtContinue (Just name') :&&.: NoAnn s p
        NoAnnView x p -> Renamer $ defaultCase x p
    where defaultCase x p = fmap (:&&.: NoAnn s p) $ htraverse (getRenamer . fsnd) x

rename = getRenamer . para renameAlg
