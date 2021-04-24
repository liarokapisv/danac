{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Danac.TypeChecker where

import Data.Comp.Multi.Algebra
import Data.Comp.Multi.Term
import Data.Comp.Multi.HTraversable (htraverse)
import Data.Validation

import Danac.Ast
import Danac.Util.Annotation
import Danac.Util.SourceSpan
import qualified Danac.Renamer as RN
import Data.Text (Text, length)
import Data.Maybe (maybeToList, catMaybes)
import Data.List (find)
import GHC.Show (appPrec, appPrec1)

data LvalueType = LType Type | LPointer Type
    deriving (Eq, Show)
data ValueType = RValue DataType | LValue LvalueType
    deriving (Eq, Show)

toLvalueType :: RN.VarType -> LvalueType
toLvalueType (RN.Param t) = parToLvalue t
toLvalueType (RN.Local t) = LType t

parToLvalue :: FparType -> LvalueType
parToLvalue (Pointer x) = LPointer x
parToLvalue (Value x) = LType x
parToLvalue (Ref x) = LType (DType x)


data Eval i where
    EvalVariable :: SourceSpan -> RN.VarType -> Eval VarIdentifier
    EvalHeader :: SourceSpan -> Maybe DataType -> Eval Header
    EvalFunction :: Text -> Maybe SourceSpan -> FunctionType -> Eval FuncIdentifier
    EvalLvalue :: SourceSpan -> LvalueType -> Eval Lvalue
    EvalExpr :: SourceSpan -> ValueType -> Eval Expr
    EvalFuncCall :: SourceSpan -> Text -> Maybe DataType -> Eval FuncCall
    EvalCondStmt :: Maybe (SourceSpan, Maybe DataType) -> Eval CondStmt
    EvalStmt :: Maybe (SourceSpan, Maybe DataType) -> Eval Stmt
    EvalBlock :: SourceSpan -> Maybe (SourceSpan, Maybe DataType) -> Eval Block
    NoEval :: !(NoEvalGroup i) -> Eval i

data NoEvalGroup i where
    NoEvalAst :: NoEvalGroup Ast
    NoEvalIdentifier :: NoEvalGroup Identifier
    NoEvalFuncDef :: NoEvalGroup FuncDef
    NoEvalFparDef :: NoEvalGroup FparDef
    NoEvalLocalDef :: NoEvalGroup LocalDef
    NoEvalFuncDecl :: NoEvalGroup FuncDecl
    NoEvalVarDef :: NoEvalGroup VarDef
    NoEvalCond :: NoEvalGroup Cond

data View r i where
    VariableView :: T r VarIdentifier -> View r VarIdentifier
    HeaderView :: T r Header -> View r Header
    FunctionView :: T r FuncIdentifier -> View r FuncIdentifier
    LvalueView :: T r Lvalue -> View r Lvalue
    ExprView :: T r Expr -> View r Expr
    FuncCallView :: T r FuncCall -> View r FuncCall
    CondStmtView :: T r CondStmt -> View r CondStmt
    StmtView :: T r Stmt -> View r Stmt
    BlockView :: T r Block -> View r Block
    NoEvalView :: T r i -> NoEvalGroup i -> View r i

view :: T r i -> View r i
view y = case group y of
    GroupFuncIdentifier x -> FunctionView x
    GroupVarIdentifier x -> VariableView x
    GroupIdentifier x -> NoEvalView x NoEvalIdentifier
    GroupCondStmt x -> CondStmtView x
    GroupStmt x -> StmtView x
    GroupBlock x -> BlockView x
    GroupFuncCall x -> FuncCallView x
    GroupLvalue x -> LvalueView x
    GroupExpr x -> ExprView x
    GroupAst x -> NoEvalView x NoEvalAst
    GroupFuncDef x -> NoEvalView x NoEvalFuncDef
    GroupHeader x -> HeaderView x
    GroupFparDef x -> NoEvalView x NoEvalFparDef
    GroupLocalDef x -> NoEvalView x NoEvalLocalDef
    GroupFuncDecl x -> NoEvalView x NoEvalFuncDecl
    GroupVarDef x -> NoEvalView x NoEvalVarDef
    GroupCond x -> NoEvalView x NoEvalCond

data Error = LvalueAxLhsWrongType SourceSpan LvalueType
           | LvalueAxRhsNotIntegral SourceSpan ValueType
           | ExprIncompatibleTypes (SourceSpan, ValueType) (SourceSpan, ValueType)
           | ExprProcedureCallNotAllowed SourceSpan Text
           | FuncCallWrongNumberOfParameters SourceSpan Text FunctionType Integer (Maybe SourceSpan)
           | FuncCallIncompatibleArg (Integer, FparType) (SourceSpan, ValueType) (Maybe SourceSpan)
           | ReturnPointerType SourceSpan Type
           | ReturnArrayType SourceSpan Type Integer
           | PointerOnAssignLhs SourceSpan Type
           | ArrayOnAssignLhs SourceSpan Type Integer
           | AssignTypeMismatch (SourceSpan, LvalueType) (SourceSpan, ValueType)
           | InconsistentReturnTypes (SourceSpan, Maybe DataType) (SourceSpan, Maybe DataType)
           | ExitInFunctionWithReturnType SourceSpan (SourceSpan, DataType)
           | NoReturnInFunctionWithReturnType (SourceSpan, DataType)
           | WrongReturnType SourceSpan (SourceSpan, DataType) (SourceSpan, DataType)
           | ReturnInFunctionWithNoReturnType SourceSpan (SourceSpan, DataType)
    deriving Show

type Validated = Validation [Error]

type TC = F Validated Eval

typecheckAlg :: Alg (T :&&: RN.Ann) TC
typecheckAlg (w :&&: ann) = 
    case view w of
        VariableView _ -> case ann of
                            RN.AnnVariable s (_,_,_, t) -> F $ Success $ EvalVariable s t
        FunctionView (FuncIdentifier n) -> case ann of
                            RN.AnnFunction _ (_,s',t) -> F $ Success $ EvalFunction n s' t
        LvalueView (LvalueId (F t)) -> F $ fmap (\(EvalVariable s x) -> EvalLvalue s $ toLvalueType x) t
        LvalueView (LvalueStr t) -> let RN.NoAnn s _ = ann in F $ Success $ EvalLvalue s $ LType $ AType (DType Byte) (toInteger (Data.Text.length t) + 1)
        LvalueView (LvalueAx (F l) (F i)) -> F $ bindValidation (go <$> l <*> i) id
            where go :: Eval Lvalue -> Eval Expr -> Validated (Eval Lvalue)
                  go (EvalLvalue sl tl) (EvalExpr sr tr) = 
                    case (derefType tl, isIntegralType tr) of
                         (Nothing, False) -> Failure $ [LvalueAxLhsWrongType sl tl, LvalueAxRhsNotIntegral sr tr]
                         (Nothing, True) -> Failure $ [LvalueAxLhsWrongType sl tl]
                         (Just _, False) -> Failure $ [LvalueAxRhsNotIntegral sr tr]
                         (Just t, True) -> let RN.NoAnn s _ = ann in Success $ EvalLvalue s t
                  derefType :: LvalueType -> Maybe LvalueType
                  derefType (LType (DType _)) = Nothing
                  derefType (LType (AType t _)) = Just $ LType t
                  derefType (LPointer t) = Just $ LType t
                  isIntegralType :: ValueType -> Bool
                  isIntegralType (RValue Integ) = True
                  isIntegralType (LValue (LType (DType Integ))) = True
                  isIntegralType _ = False
        ExprView z ->
            let RN.NoAnn s _ = ann in
                case z of
                    ExprInt _ -> F $ Success $ EvalExpr s $ RValue Integ
                    ExprChar _ -> F $ Success $ EvalExpr s $ RValue Byte
                    ExprLvalue (F x) -> F $ fmap (\(EvalLvalue _ t) -> EvalExpr s (LValue t)) x
                    ExprAdd lhs rhs-> binOp s lhs rhs
                    ExprSub lhs rhs-> binOp s lhs rhs
                    ExprMul lhs rhs-> binOp s lhs rhs
                    ExprDiv lhs rhs-> binOp s lhs rhs
                    ExprMod lhs rhs-> binOp s lhs rhs
                    ExprOr  lhs rhs-> binOp s lhs rhs
                    ExprAnd lhs rhs-> binOp s lhs rhs
                    ExprMinus (F x) -> F $ fmap (\(EvalExpr _ t) -> EvalExpr s t) x
                    ExprNot (F x) -> F $ fmap (\(EvalExpr _ t) -> EvalExpr s t) x
                    ExprTrue -> F $ Success $ EvalExpr s $ RValue Byte
                    ExprFalse -> F $ Success $ EvalExpr s $ RValue Byte
                    ExprFuncCall (F x) -> F $ bindValidation (validFuncCall <$> x) id
                    where validFuncCall :: Eval FuncCall -> Validated (Eval Expr)
                          validFuncCall (EvalFuncCall s t Nothing) = Failure $ [ExprProcedureCallNotAllowed s t]
                          validFuncCall (EvalFuncCall s _ (Just dt)) = Success $ EvalExpr s $ RValue dt
                          binOp s (F lhs) (F rhs) = F $ bindValidation (binOp' s <$> lhs <*> rhs) id 
                          binOp' :: SourceSpan -> Eval Expr -> Eval Expr -> Validated (Eval Expr)
                          binOp' s (EvalExpr sl tl) (EvalExpr sr tr) =
                                    case compatibleTypes tl tr of
                                         Nothing -> Failure $ [ExprIncompatibleTypes (sl, tl) (sr, tr)]
                                         Just dt -> Success $ EvalExpr s $ RValue dt
                          compatibleTypes :: ValueType -> ValueType -> Maybe DataType
                          compatibleTypes (RValue lhs) (RValue rhs) | lhs == rhs = Just lhs
                          compatibleTypes (RValue lhs) (LValue (LType (DType rhs))) | lhs == rhs = Just lhs
                          compatibleTypes (LValue (LType (DType lhs))) (RValue rhs) | lhs == rhs = Just lhs
                          compatibleTypes (LValue (LType (DType lhs))) (LValue (LType (DType rhs))) | lhs == rhs = Just lhs
                          compatibleTypes _ _ = Nothing
        FuncCallView (FuncCall (F name) es)  -> let RN.NoAnn s _ = ann in F $ bindValidation (go s <$> name <*> (traverse unF es)) id
                where go :: SourceSpan -> Eval FuncIdentifier -> [Eval Expr] -> Validated (Eval FuncCall)
                      go s (EvalFunction t s' ft@(FunctionType mdt fts)) es' =
                        case areCompatible 0 fts es' of
                                [] -> Success $ EvalFuncCall s t mdt
                                errs -> Failure errs
                        where areCompatible _ [] [] = []
                              areCompatible n [] xs = [FuncCallWrongNumberOfParameters s t ft (n + toInteger (Prelude.length xs)) s']
                              areCompatible n _ [] = [FuncCallWrongNumberOfParameters s t ft n s']
                              areCompatible n (f : fs) (EvalExpr _ vt : xs) | isCompatible f vt = areCompatible (n+1) fs xs
                              areCompatible n (f : fs) (EvalExpr sr vt : xs) | otherwise = FuncCallIncompatibleArg (n, f) (sr, vt) s' : areCompatible (n+1) fs xs
                              isCompatible (Ref l) (LValue (LType (DType r))) = l == r
                              isCompatible (Ref _) _ = False
                              isCompatible (Value (DType l)) (RValue r) = l == r
                              isCompatible (Value _) (RValue _) = False
                              isCompatible (Value l) (LValue (LType r)) = l == r
                              isCompatible (Value _) (LValue (LPointer _)) = False
                              isCompatible (Pointer _) (RValue _) = False
                              isCompatible (Pointer l) (LValue (LType (AType r _))) = l == r
                              isCompatible (Pointer l) (LValue (LPointer r)) = l == r
                              isCompatible (Pointer _) (LValue _) = False
        StmtView z -> 
            let RN.NoAnn s _ = ann in
                case z of
                    StmtSkip -> F $ Success $ EvalStmt Nothing
                    StmtBreak _ -> F $ Success $ EvalStmt Nothing
                    StmtContinue _ -> F $ Success $ EvalStmt Nothing
                    StmtProcCall (F c) -> F $ fmap (\(EvalFuncCall _ _ _) -> EvalStmt Nothing) c
                    StmtExit -> F $ Success $ EvalStmt (Just (s, Nothing))
                    StmtReturn (F e) -> F $ bindValidation (validRetType <$> e) id
                        where validRetType :: Eval Expr -> Validated (Eval Stmt)
                              validRetType (EvalExpr sr (RValue dt)) = Success $ EvalStmt (Just (sr, Just dt))
                              validRetType (EvalExpr sr (LValue (LPointer t))) = Failure $ [ReturnPointerType sr t]
                              validRetType (EvalExpr sr (LValue (LType (AType t i)))) = Failure $ [ReturnArrayType sr t i]
                              validRetType (EvalExpr sr (LValue (LType (DType dt)))) = Success $ EvalStmt (Just (sr, Just dt))
                    StmtLoop _ (F b) -> F $ fmap (\(EvalBlock _ mdt) -> EvalStmt mdt) b
                    StmtAssign (F lhs) (F rhs) -> F $ bindValidation (go <$> lhs <*> rhs) id
                        where go :: Eval Lvalue -> Eval Expr -> Validated (Eval Stmt)
                              go (EvalLvalue _ (LType (DType l))) (EvalExpr _ (RValue r)) | l == r = Success $ EvalStmt Nothing
                              go (EvalLvalue _ (LType (DType l))) (EvalExpr _ (LValue (LType (DType r)))) | l == r = Success $ EvalStmt Nothing
                              go (EvalLvalue sl (LType (AType t i))) _ = Failure $ [ArrayOnAssignLhs sl t i]
                              go (EvalLvalue sl (LPointer t)) _ = Failure $ [PointerOnAssignLhs sl t]
                              go (EvalLvalue sl tl) (EvalExpr sr tr) = Failure $ [AssignTypeMismatch (sl,tl) (sr,tr)]
                    StmtIf (F c) cs mb -> F $ bindValidation (go <$> c <*> traverse unF cs <*> traverse unF mb) id
                        where go :: Eval CondStmt -> [Eval CondStmt] -> Maybe (Eval Block) -> Validated (Eval Stmt)
                              go (EvalCondStmt x) ys mz = 
                                case validStmts (catMaybes $ [x] ++ fmap (\(EvalCondStmt t) -> t) ys ++ maybeToList (fmap (\(EvalBlock _ t) -> t) mz)) of
                                    Left err -> Failure $ [err]
                                    Right mdt -> Success $ EvalStmt mdt
        CondStmtView (CondStmt _ (F b)) -> F $ fmap (\(EvalBlock _ t) -> EvalCondStmt t) b
        BlockView (Block stmts) -> 
            let RN.NoAnn s _ = ann
                go xs = case validStmts (catMaybes $ fmap (\(EvalStmt t) -> t) xs) of
                                    Left err -> Failure $ [err]
                                    Right mdt -> Success $ EvalBlock s mdt
                    in F $ bindValidation (go <$> traverse unF stmts) id
        HeaderView (Header _ mdt _) -> 
            let RN.NoAnn s _ = ann in F $ Success $ EvalHeader s mdt
        NoEvalView (FuncDef (F h) lds (F b)) p -> 
            let RN.NoAnn s _ = ann 
                go :: Eval Header -> [Eval LocalDef] -> Eval Block -> Validated (Eval FuncDef)
                go (EvalHeader _ (Just x)) _ (EvalBlock _ (Just (_, Just y))) | x == y = Success $ NoEval p
                go (EvalHeader sl (Just x)) _ (EvalBlock _ (Just (sr, Just y))) | otherwise = Failure $ [WrongReturnType s (sl, x) (sr, y)]
                go (EvalHeader sl (Just x)) _ (EvalBlock _ (Just (sr,Nothing))) = Failure $ [ExitInFunctionWithReturnType sr (sl, x)]
                go (EvalHeader _ Nothing) _ (EvalBlock _ (Just (_,Nothing))) = Success $ NoEval p
                go (EvalHeader sl Nothing) _ (EvalBlock _ (Just (sr,Just y))) = Failure $ [ReturnInFunctionWithNoReturnType sr (sl, y)]
                go (EvalHeader _ Nothing) _ (EvalBlock _ Nothing) = Success $ NoEval p
                go (EvalHeader sl (Just x)) _ (EvalBlock _ Nothing) = Failure $ [NoReturnInFunctionWithReturnType (sl, x)]
                    in F $ bindValidation (go <$> h <*> traverse unF lds <*> b) id
        NoEvalView x p -> F $ htraverse unF x *> (Success $ NoEval p)
        where validStmts :: [(SourceSpan, Maybe DataType)] -> Either Error (Maybe (SourceSpan, Maybe DataType))
              validStmts [] = Right Nothing
              validStmts (x@(s, t) : xs) =
                case find ((/= t).snd) xs of
                    Nothing -> Right $ Just (s, t)
                    Just y -> Left $ InconsistentReturnTypes x y

data NoAnnGroup i where
    NoAnnAst :: NoAnnGroup Ast
    NoAnnHeader :: NoAnnGroup Header
    NoAnnLvalue :: NoAnnGroup Lvalue
    NoAnnIdentifier :: NoAnnGroup Identifier
    NoAnnFuncCall :: NoAnnGroup FuncCall
    NoAnnFuncDef :: NoAnnGroup FuncDef
    NoAnnFparDef :: NoAnnGroup FparDef
    NoAnnLocalDef :: NoAnnGroup LocalDef
    NoAnnFuncDecl :: NoAnnGroup FuncDecl
    NoAnnVarDef :: NoAnnGroup VarDef
    NoAnnCond :: NoAnnGroup Cond
    NoAnnCondStmt :: NoAnnGroup CondStmt
    NoAnnStmt :: NoAnnGroup Stmt
    NoAnnBlock :: NoAnnGroup Block

data Ann i where
    AnnVariable :: Text -> Int -> Int -> RN.VarType -> Ann VarIdentifier
    AnnFunction :: Maybe Int -> FunctionType -> Ann FuncIdentifier
    AnnExpr :: Maybe DataType -> Ann Expr
    NoAnn :: !(NoAnnGroup i) -> Ann i

instance Show (Ann i) where
    showsPrec p (AnnVariable a b c d) = showParen (p > appPrec) $ 
                                              showString "AnnVariable " .  showsPrec appPrec1 a . 
                                              showString " " .  showsPrec appPrec1 b . 
                                              showString " " .  showsPrec appPrec1 c . 
                                              showString " " .  showsPrec appPrec1 d
    showsPrec p (AnnFunction a b) = showParen (p > appPrec) $
                                           showString "AnnFunction " .  showsPrec appPrec1 a .
                                           showString " " .  showsPrec appPrec1 b
    showsPrec p (AnnExpr a) = showParen (p > appPrec) $ 
                                    showString "AnnExpr " .  showsPrec appPrec1 a
    showsPrec _ _ = showString ""

extractDataType :: ValueType -> Maybe DataType
extractDataType (RValue t) = Just t
extractDataType (LValue (LType (DType t))) = Just t
extractDataType _ = Nothing

mergeAnns :: RN.Ann i -> Eval i -> Ann i
mergeAnns (RN.AnnVariable _ (x, y, z, t)) (EvalVariable _ _) = AnnVariable x y z t
mergeAnns (RN.AnnFunction _ (x, _, _)) (EvalFunction _ _ y) = AnnFunction x y
mergeAnns (RN.NoAnn _ RN.NoAnnExpr) (EvalExpr _ t) = AnnExpr $ extractDataType t
mergeAnns (RN.NoAnn _ RN.NoAnnHeader) (EvalHeader _ _) = NoAnn NoAnnHeader
mergeAnns (RN.NoAnn _ RN.NoAnnLvalue) (EvalLvalue _ _) = NoAnn NoAnnLvalue
mergeAnns (RN.NoAnn _ RN.NoAnnFuncCall) (EvalFuncCall _ _ _) = NoAnn NoAnnFuncCall
mergeAnns (RN.NoAnn _ _) (NoEval NoEvalIdentifier) = NoAnn NoAnnIdentifier
mergeAnns (RN.NoAnn _ _) (NoEval NoEvalAst) = NoAnn NoAnnAst
mergeAnns (RN.NoAnn _ _) (NoEval NoEvalFuncDef) = NoAnn NoAnnFuncDef
mergeAnns (RN.NoAnn _ _) (NoEval NoEvalFparDef) = NoAnn NoAnnFparDef
mergeAnns (RN.NoAnn _ _) (NoEval NoEvalLocalDef) = NoAnn NoAnnLocalDef
mergeAnns (RN.NoAnn _ _) (NoEval NoEvalFuncDecl) = NoAnn NoAnnFuncDecl
mergeAnns (RN.NoAnn _ _) (NoEval NoEvalVarDef) = NoAnn NoAnnVarDef
mergeAnns (RN.NoAnn _ _) (NoEval NoEvalCond) = NoAnn NoAnnCond
mergeAnns (RN.NoAnn _ _) (EvalCondStmt _) = NoAnn NoAnnCondStmt
mergeAnns (RN.NoAnn _ _) (EvalStmt _) = NoAnn NoAnnStmt
mergeAnns (RN.NoAnn _ _) (EvalBlock _ _) = NoAnn NoAnnBlock

typecheck :: Term (T :&&: RN.Ann) i -> Either [Error] (Term (T :&&: Ann) i)
typecheck x = case (unF $ extend bindValidation typecheckAlg x) of
                Failure errs -> Left errs
                Success t -> Right $ combine mergeAnns t
