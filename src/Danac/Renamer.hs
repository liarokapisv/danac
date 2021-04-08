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
{-# LANGUAGE TupleSections #-}


module Danac.Renamer where

import Danac.Ast
import Danac.Util.Annotation
import Danac.Util.SourceSpan
import Danac.Util.Helpers (locally)
import Data.Text (Text)
import Data.Validation
import Control.Monad.State
import Data.Comp.Multi.Ops (ffst, fsnd, (:*:)(..))
import Data.Comp.Multi.Algebra
import Data.Comp.Multi.Term (Term)
import Data.Comp.Multi.HTraversable (htraverse)
import Data.Functor.Compose
import Data.List (find, intersperse)
import GHC.Show (appPrec, appPrec1)

data Error = UndefinedVariable Text SourceSpan
           | UndefinedFunction Text SourceSpan
           | UndefinedLabel Text SourceSpan
           | AlreadyDefinedVariable Text SourceSpan SourceSpan
           | AlreadyDefinedFunction Text SourceSpan (Maybe SourceSpan)
           | AlreadyDefinedLabel Text SourceSpan SourceSpan
    deriving Show

data VarType = Param FparType | Local Type
    deriving Show

data VarInfo = VarInfo {
    vname :: Text,
    vtype :: VarType,
    vspan :: SourceSpan
}

data FuncInfo = FuncInfo {
    fname :: Text,
    ftype :: FunctionType,
    fspan :: Maybe SourceSpan
}

data LabelInfo = LabelInfo {
    lname :: Text,
    lspan :: SourceSpan
}

data Frame = Frame {
    functionName :: Text,
    functionType :: FunctionType,
    variables :: [VarInfo],
    functions :: [FuncInfo]
}

data Context = Context {
    frames :: [Frame],
    globals :: [FuncInfo],
    labels :: [LabelInfo]
}

emptyContext = Context { frames = [], 
                         globals = [FuncInfo { fname = "writeInteger", ftype = FunctionType Nothing [Value (DType Integ)], fspan = Nothing },
                                    FuncInfo { fname = "writeByte", ftype = FunctionType Nothing [Value (DType Byte)], fspan = Nothing },
                                    FuncInfo { fname = "writeChar", ftype = FunctionType Nothing [Value (DType Byte)], fspan = Nothing },
                                    FuncInfo { fname = "writeString", ftype = FunctionType Nothing [Pointer (DType Byte)], fspan = Nothing },
                                    FuncInfo { fname = "readInteger", ftype = FunctionType (Just Integ) [], fspan = Nothing },
                                    FuncInfo { fname = "readByte", ftype = FunctionType (Just Byte) [], fspan = Nothing },
                                    FuncInfo { fname = "readChar", ftype = FunctionType (Just Byte) [], fspan = Nothing },
                                    FuncInfo { fname = "readString", ftype = FunctionType Nothing [Value (DType Integ), Pointer (DType Byte)], fspan = Nothing },
                                    FuncInfo { fname = "extend", ftype = FunctionType (Just Integ) [Value (DType Byte)], fspan = Nothing },
                                    FuncInfo { fname = "shrink", ftype = FunctionType (Just Byte) [Value (DType Integ)], fspan = Nothing },
                                    FuncInfo { fname = "strlen", ftype = FunctionType (Just Integ) [Pointer (DType Byte)], fspan = Nothing },
                                    FuncInfo { fname = "strcmp", ftype = FunctionType (Just Integ) [Pointer (DType Byte), Pointer (DType Byte)], fspan = Nothing },
                                    FuncInfo { fname = "strcpy", ftype = FunctionType Nothing [Pointer (DType Byte), Pointer (DType Byte)], fspan = Nothing },
                                    FuncInfo { fname = "strcat", ftype = FunctionType Nothing [Pointer (DType Byte), Pointer (DType Byte)], fspan = Nothing }],
                         labels=[] }

withNamespace t names = mconcat $ intersperse "." $ reverse $ (t : names)

lookVariable :: Text -> Context -> Maybe (Text, Int, Int, VarType)
lookVariable t c = go 0 (frames c)
    where go _ [] = Nothing
          go n (Frame {functionName = name, variables = vs} : fs) = 
                case find ((==t).vname.snd) (zip [0..] vs) of
                        Just (i, VarInfo {vtype=typ}) -> Just (withNamespace name (fmap functionName fs), n, i, typ)
                        Nothing -> go (n+1) fs

declareFunction :: Text -> FunctionType -> SourceSpan -> Context -> Context
declareFunction n t s c = 
    case frames c of
            [] -> c
            (f : fs) -> c { frames = appendFunction f : fs }
    where appendFunction :: Frame -> Frame
          appendFunction f = f { functions = FuncInfo { fname = n, ftype = t, fspan = Just s } : functions f }

lookFunction :: Text -> Context -> Maybe (Text, Maybe Int, FunctionType)
lookFunction t c = case go 0 (frames c) of
                        Just n -> Just n
                        Nothing -> case find ((==t).fname) (globals c) of
                                      Just (FuncInfo { ftype = typ }) -> Just (t, Nothing, typ)
                                      Nothing -> Nothing
    where go _ [] = Nothing
          go n fs@(f : fs') = case find ((==t).fname) (functions f) of
                                 Just (FuncInfo { ftype = typ }) -> Just $ (withNamespace t (fmap functionName fs), Just n, typ)
                                 Nothing | t == functionName f -> Just (withNamespace t (fmap functionName fs'), Just (n+1), functionType f)
                                         | otherwise -> go (n+1) fs'

lookLabel :: Text -> Context -> Maybe Text
lookLabel t c = case dropWhile ((/= t).lname) (labels c) of
                     [] -> Nothing
                     (x:xs) -> Just $ withNamespace (lname x) (fmap lname xs)

getFparTypes :: [Term (T :&: SourceSpan) FparDef] -> [FparType]
getFparTypes = fmap (\(FparDef _ t :&.: _) -> t)

headerToType :: Term (T :&: SourceSpan) Header -> FunctionType
headerToType (Header _ dt fdefs :&.: _) = FunctionType dt $ getFparTypes fdefs

extractName :: Term (T :&: SourceSpan) Header -> (Text, SourceSpan)
extractName (Header (Identifier t :&.: s) _ _ :&.: _) = (t, s)
                
collectHeaderVarNames :: Term (T :&: SourceSpan) Header -> Either Error (Text, SourceSpan, FunctionType, [VarInfo])
collectHeaderVarNames h@(Header (Identifier name :&.: s) _ fdefs :&.: _) = fmap (name, s, headerToType h,) $ go [] fdefs
    where go :: [VarInfo] -> [Term (T :&: SourceSpan) FparDef] -> Either Error [VarInfo]
          go ns [] = Right ns
          go ns (FparDef (Identifier n' :&.: s') t :&.: _ : fs) = 
                    case find ((==n').vname) ns of
                        Just n'' -> Left $ AlreadyDefinedVariable n' s (vspan n'')
                        Nothing -> go (VarInfo { vname = n', vtype = Param t, vspan = s' } : ns) fs


collectLocalNames :: ([VarInfo],[FuncInfo]) -> [Term (T :&: SourceSpan) LocalDef] -> Either Error ([VarInfo],[FuncInfo])
collectLocalNames (vars,fns) [] = Right (vars, fns)
collectLocalNames (vars,fns) (LocalDefFuncDef (FuncDef h@(Header (Identifier t :&.: s) _ _ :&.: _) _ _ :&.: _) :&.: _ : ds) =
    case find ((==t).fname) fns of
        Just f -> Left $ AlreadyDefinedFunction t s (fspan f)
        Nothing -> collectLocalNames (vars,(FuncInfo { fname = t, ftype = headerToType h, fspan = Just s } : fns)) ds
collectLocalNames (vars,fns) (LocalDefFuncDecl (FuncDecl h@(Header (Identifier t :&.: s) _ _ :&.: _) :&.: _) :&.: _ : ds) =
    case find ((==t).fname) fns of
        Just f -> Left $ AlreadyDefinedFunction t s (fspan f)
        Nothing -> collectLocalNames (vars,(FuncInfo { fname = t, ftype = headerToType h, fspan = Just s } : fns)) ds
collectLocalNames (vars,fns) (LocalDefVarDef (VarDef (Identifier name :&.: _) t :&.: s) :&.: _ : ds) =
    case find ((==name).vname) vars of
        Just v -> Left $ AlreadyDefinedVariable name s (vspan v)
        Nothing -> collectLocalNames ((VarInfo {vname = name, vtype = Local t, vspan = s } : vars),fns) ds

createFrame :: Term (T :&: SourceSpan) Header -> [Term (T :&: SourceSpan) LocalDef] -> Either Error (Text, SourceSpan, Frame)
createFrame header ldefs = do
    (name, s, typ, vars1) <- collectHeaderVarNames header
    (vars2, fns) <- collectLocalNames (vars1,[]) ldefs
    pure $ (name, s, Frame { functionName = name, functionType = typ, variables = reverse vars2, functions = reverse fns })


data NoAnnGroup i where
    NoAnnAst :: NoAnnGroup Ast
    NoAnnIdentifier :: NoAnnGroup Identifier
    NoAnnFuncDef :: NoAnnGroup FuncDef
    NoAnnHeader :: NoAnnGroup Header
    NoAnnFparDef :: NoAnnGroup FparDef
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

data Ann i where
    AnnVariable :: SourceSpan -> (Text, Int, Int, VarType) -> Ann VarIdentifier
    AnnFunction :: SourceSpan -> (Maybe Int, FunctionType) -> Ann FuncIdentifier
    NoAnn :: SourceSpan -> !(NoAnnGroup i) -> Ann i

instance Show (Ann i) where
    showsPrec p (AnnVariable x y) = showParen (p > appPrec) $ showString "AnnVariable " . showsPrec appPrec1 x . showsPrec appPrec1 y
    showsPrec p (AnnFunction x y) = showParen (p > appPrec) $ showString "AnnFunction " . showsPrec appPrec1 x . showsPrec appPrec1 y
    showsPrec _ _ = showString ""

data View r i where
    VariableView :: T r VarIdentifier -> View r VarIdentifier
    FunctionView :: T r FuncIdentifier -> View r FuncIdentifier
    NoAnnView :: T r i -> NoAnnGroup i -> View r i

view :: T r i -> View r i
view y = case group y of
    GroupFuncIdentifier x -> FunctionView x
    GroupVarIdentifier x -> VariableView x
    GroupIdentifier x -> NoAnnView x NoAnnIdentifier
    GroupAst x -> NoAnnView x NoAnnAst
    GroupFuncDef x -> NoAnnView x NoAnnFuncDef
    GroupHeader x -> NoAnnView x NoAnnHeader
    GroupFparDef x -> NoAnnView x NoAnnFparDef
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

newtype Renamer f i = Renamer { getRenamer :: Compose (State Context) (Validation [Error]) (f i) }

renameHeaderFDef :: Text -> Term (T :&&: Ann) FuncDef -> Term (T :&&: Ann) FuncDef
renameHeaderFDef t (FuncDef (Header (Identifier _ :&&.: s) x y :&&.: z) k l :&&.: m) = FuncDef (Header (Identifier t :&&.: s) x y :&&.: z) k l :&&.: m

renameHeaderFDecl :: Text -> Term (T :&&: Ann) FuncDecl -> Term (T :&&: Ann) FuncDecl
renameHeaderFDecl t (FuncDecl (Header (Identifier _ :&&.: s) x y :&&.: z) :&&.: m) = FuncDecl (Header (Identifier t :&&.: s) x y :&&.: z) :&&.: m

renameLoopLabel :: Text -> Term (T :&&: Ann) Stmt -> Term (T :&&: Ann) Stmt
renameLoopLabel t (StmtLoop (Just (Identifier _ :&&.: s)) xs :&&.: y) = StmtLoop (Just (Identifier t :&&.: s)) xs :&&.: y
renameLoopLabel _ x = x

renameAlg :: RAlg (T :&: SourceSpan) (Renamer (Term (T :&&: Ann))) 
renameAlg (t :&: s) = 
    case view t of
        VariableView (VarIdentifier name) -> Renamer $ Compose $ do
            minfo <- gets $ lookVariable name
            case minfo of
                Nothing -> pure $ Failure [UndefinedVariable name s]
                Just info -> pure $ Success $ VarIdentifier name :&&.: AnnVariable s info
        FunctionView (FuncIdentifier name) -> Renamer $ Compose $ do
            mf <- gets $ lookFunction name 
            case mf of
                Nothing -> pure $ Failure [UndefinedFunction name s]
                Just (name',n,typ) -> pure $ Success $ (FuncIdentifier name' :&&.: AnnFunction s (n,typ))
        NoAnnView f@(FuncDef (header :*: _) ldefs _) p -> Renamer $ Compose $ do
            let mframe = createFrame header (fmap ffst ldefs)
            case mframe of
                Left e -> pure $ Failure [e]
                Right (name, s', frame) -> do
                    name' <- gets $ withNamespace name . fmap functionName . frames
                    modify $ declareFunction name' (functionType frame) s'
                    res <- locally $ do
                            modify (\r -> r { frames = frame : frames r})
                            getCompose $ fmap (renameHeaderFDef name') $ defaultCase f p
                    pure res
        NoAnnView f@(FuncDecl (header :*: _)) p -> Renamer $ Compose $ do
            let (name, s') = extractName header
            name' <- gets $ withNamespace name . fmap functionName . frames
            let typ = headerToType header
            res <- getCompose $ fmap (renameHeaderFDecl name') $ defaultCase f p
            modify $ declareFunction name' typ s'
            pure res
        NoAnnView l@(StmtLoop (Just ((Identifier name :&.: s') :*: _)) _) p -> Renamer $ Compose $ do
            ml <- gets $ find ((== name) . lname) . labels
            case ml of
                Nothing -> do
                    name' <- gets $ withNamespace name . fmap lname . labels
                    modify (\r -> r { labels = LabelInfo { lname = name, lspan = s'} : labels r })
                    locally $ getCompose $ fmap (renameLoopLabel name') $ defaultCase l p
                Just l' -> pure $ Failure [AlreadyDefinedLabel name s' (lspan l')]
        NoAnnView (StmtBreak (Just ((Identifier name :&.: s') :*: _))) p -> Renamer $ Compose $ do
            mlabel <- gets $ lookLabel name
            case mlabel of
                Nothing -> pure $ Failure [UndefinedLabel name s']
                Just name' -> pure $ Success $ StmtBreak (Just (Identifier name' :&&.: NoAnn s' NoAnnIdentifier)) :&&.: NoAnn s p
        NoAnnView (StmtContinue (Just ((Identifier name :&.: s') :*: _))) p -> Renamer $ Compose $ do
            mlabel <- gets $ lookLabel name
            case mlabel of
                Nothing -> pure $ Failure [UndefinedLabel name s']
                Just name' -> pure $ Success $ StmtContinue (Just (Identifier name' :&&.: NoAnn s' NoAnnIdentifier)) :&&.: NoAnn s p
        NoAnnView x p -> Renamer $ defaultCase x p
    where defaultCase x p = fmap (:&&.: NoAnn s p) $ htraverse (getRenamer . fsnd) x

rename x = case evalState (getCompose $ getRenamer $ para renameAlg x) emptyContext of
                Failure errs -> Left errs
                Success t -> Right t
