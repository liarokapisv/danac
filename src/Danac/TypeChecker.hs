{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Danac.TypeChecker where

import Data.Comp.Multi.Algebra
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.Term
import Data.Validation

import Danac.Ast
import Danac.Util.Annotation
import Danac.Util.SourceSpan
import qualified Danac.Renamer as RN
import Data.Text (Text, length)
import Data.Maybe (maybeToList, catMaybes)

data LvalueType = LType Type | LPointer Type
    deriving (Eq, Show)
data ValueType = RValue DataType | LValue LvalueType
    deriving (Eq, Show)

toLvalueType :: FancyType -> LvalueType
toLvalueType (Pointer x) = LPointer x
toLvalueType (Value x) = LType x
toLvalueType (Ref x) = LType (DType x)

data Eval i where
    EvalVariable :: SourceSpan -> LvalueType -> Eval VarIdentifier
    EvalHeader :: SourceSpan -> Maybe DataType -> Eval Header
    EvalFunction :: SourceSpan -> Text -> FunctionType -> Eval FuncIdentifier
    EvalLvalue :: SourceSpan -> LvalueType -> Eval Lvalue
    EvalExpr :: SourceSpan -> ValueType -> Eval Expr
    EvalFuncCall :: SourceSpan -> Text -> Maybe DataType -> Eval FuncCall
    EvalCondStmt :: SourceSpan -> Maybe (Maybe DataType) -> Eval CondStmt
    EvalStmt :: SourceSpan -> Maybe (Maybe DataType) -> Eval Stmt
    EvalBlock :: SourceSpan -> Maybe (Maybe DataType) -> Eval Block
    NoEval :: SourceSpan -> !(NoEvalGroup i) -> Eval i
    
data NoEvalGroup i where
    NoEvalAst :: NoEvalGroup Ast
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
           | ExprIncompatibleTypes SourceSpan (SourceSpan, ValueType) (SourceSpan, ValueType)
           | ExprProcedureCallNotAllowed SourceSpan Text
           | FuncCallWrongNumberOfParameters SourceSpan Text FunctionType Integer
           | FuncCallIncompatibleArg SourceSpan (Integer, FancyType) (SourceSpan, ValueType)
           | ReturnPointerType SourceSpan Type
           | ReturnArrayType SourceSpan Type Integer
           | PointerOnAssignLhs SourceSpan
           | ArrayOnAssignLhs SourceSpan
           | AssignTypeMismatch SourceSpan (SourceSpan, LvalueType) (SourceSpan, ValueType)
           | InconsistentReturnTypes SourceSpan
           | NoReturnInFunctionWithReturnType SourceSpan (SourceSpan, DataType)
           | WrongReturnType SourceSpan (SourceSpan, DataType) (SourceSpan, DataType)
           | ReturnInFunctionWithNoReturnType SourceSpan (SourceSpan, DataType)
    deriving Show

type Validated = Validation [Error]

newtype TC i = TC { unTC :: Validated (Eval i) }

typecheckAlg :: Alg (T :&&: RN.Ann) TC
typecheckAlg (w :&&: ann) = 
    case view w of
        VariableView _ -> case ann of
                            RN.AnnVariable s (_,_,_, t) -> TC $ Success $ EvalVariable s $ toLvalueType t
        FunctionView (FuncIdentifier n) -> case ann of
                            RN.AnnFunction s t -> TC $ Success $ EvalFunction s n t
        LvalueView (LvalueId (TC t)) -> TC $ fmap (\(EvalVariable s x) -> EvalLvalue s x) t
        LvalueView (LvalueStr t) -> let RN.NoAnn s _ = ann in TC $ Success $ EvalLvalue s $ LType $ AType (DType Byte) (toInteger (Data.Text.length t) + 1)
        LvalueView (LvalueAx (TC l) (TC i)) -> TC $ bindValidation (go <$> l <*> i) id
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
                    ExprInt _ -> TC $ Success $ EvalExpr s $ RValue Integ
                    ExprChar _ -> TC $ Success $ EvalExpr s $ RValue Byte
                    ExprLvalue (TC x) -> TC $ fmap (\(EvalLvalue _ t) -> EvalExpr s (LValue t)) x
                    ExprAdd lhs rhs-> binOp s lhs rhs
                    ExprSub lhs rhs-> binOp s lhs rhs
                    ExprMul lhs rhs-> binOp s lhs rhs
                    ExprDiv lhs rhs-> binOp s lhs rhs
                    ExprMod lhs rhs-> binOp s lhs rhs
                    ExprOr  lhs rhs-> binOp s lhs rhs
                    ExprAnd lhs rhs-> binOp s lhs rhs
                    ExprMinus (TC x) -> TC $ fmap (\(EvalExpr _ t) -> EvalExpr s t) x
                    ExprNot (TC x) -> TC $ fmap (\(EvalExpr _ t) -> EvalExpr s t) x
                    ExprTrue -> TC $ Success $ EvalExpr s $ RValue Byte
                    ExprFalse -> TC $ Success $ EvalExpr s $ RValue Byte
                    ExprFuncCall (TC x) -> TC $ bindValidation (validFuncCall <$> x) id
                    where validFuncCall :: Eval FuncCall -> Validated (Eval Expr)
                          validFuncCall (EvalFuncCall s t Nothing) = Failure $ [ExprProcedureCallNotAllowed s t]
                          validFuncCall (EvalFuncCall s _ (Just dt)) = Success $ EvalExpr s $ RValue dt
                          binOp s (TC lhs) (TC rhs) = TC $ bindValidation (binOp' s <$> lhs <*> rhs) id 
                          binOp' :: SourceSpan -> Eval Expr -> Eval Expr -> Validated (Eval Expr)
                          binOp' s (EvalExpr sl tl) (EvalExpr sr tr) =
                                    case compatibleTypes tl tr of
                                         Nothing -> Failure $ [ExprIncompatibleTypes s (sl, tl) (sr, tr)]
                                         Just dt -> Success $ EvalExpr s $ RValue dt
                          compatibleTypes :: ValueType -> ValueType -> Maybe DataType
                          compatibleTypes (RValue lhs) (RValue rhs) | lhs == rhs = Just lhs
                          compatibleTypes (RValue lhs) (LValue (LType (DType rhs))) | lhs == rhs = Just lhs
                          compatibleTypes (LValue (LType (DType lhs))) (RValue rhs) | lhs == rhs = Just lhs
                          compatibleTypes (LValue (LType (DType lhs))) (LValue (LType (DType rhs))) | lhs == rhs = Just lhs
                          compatibleTypes _ _ = Nothing
        FuncCallView (FuncCall (TC name) es)  -> let RN.NoAnn s _ = ann in TC $ bindValidation (go s <$> name <*> (traverse unTC es)) id
                where go :: SourceSpan -> Eval FuncIdentifier -> [Eval Expr] -> Validated (Eval FuncCall)
                      go s (EvalFunction _ t ft@(FunctionType mdt fts)) es' =
                        case areCompatible 0 fts es' of
                                [] -> Success $ EvalFuncCall s t mdt
                                errs -> Failure errs
                        where areCompatible _ [] [] = []
                              areCompatible n _ [] = [FuncCallWrongNumberOfParameters s t ft n]
                              areCompatible n [] _ = [FuncCallWrongNumberOfParameters s t ft n]
                              areCompatible n (f : fs) (EvalExpr _ vt : xs) | isCompatible f vt = areCompatible (n+1) fs xs
                              areCompatible n (f : fs) (EvalExpr sr vt : xs) | otherwise = FuncCallIncompatibleArg s (n, f) (sr, vt) : areCompatible (n+1) fs xs
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
                    StmtSkip -> TC $ Success $ EvalStmt s Nothing
                    StmtBreak _ -> TC $ Success $ EvalStmt s Nothing
                    StmtContinue _ -> TC $ Success $ EvalStmt s Nothing
                    StmtProcCall (TC c) -> TC $ fmap (\(EvalFuncCall _ _ _) -> EvalStmt s Nothing) c
                    StmtExit -> TC $ Success $ EvalStmt s (Just Nothing)
                    StmtReturn (TC e) -> TC $ bindValidation (validRetType <$> e) id
                        where validRetType :: Eval Expr -> Validated (Eval Stmt)
                              validRetType (EvalExpr _ (RValue dt)) = Success $ EvalStmt s (Just $ Just dt)
                              validRetType (EvalExpr _ (LValue (LPointer t))) = Failure $ [ReturnPointerType s t]
                              validRetType (EvalExpr _ (LValue (LType (AType t i)))) = Failure $ [ReturnArrayType s t i]
                              validRetType (EvalExpr _ (LValue (LType (DType dt)))) = Success $ EvalStmt s (Just $ Just dt)
                    StmtLoop _ (TC b) -> TC $ fmap (\(EvalBlock _ mdt) -> EvalStmt s mdt) b
                    StmtAssign (TC lhs) (TC rhs) -> TC $ bindValidation (go <$> lhs <*> rhs) id
                        where go :: Eval Lvalue -> Eval Expr -> Validated (Eval Stmt)
                              go (EvalLvalue _ (LType (DType l))) (EvalExpr _ (RValue r)) | l == r = Success $ EvalStmt s Nothing
                              go (EvalLvalue _ (LType (DType l))) (EvalExpr _ (LValue (LType (DType r)))) | l == r = Success $ EvalStmt s Nothing
                              go (EvalLvalue sl tl) (EvalExpr sr tr) = Failure $ [AssignTypeMismatch s (sl,tl) (sr,tr)]
                    StmtIf (TC c) cs mb -> TC $ bindValidation (go <$> c <*> traverse unTC cs <*> traverse unTC mb) id
                        where go :: Eval CondStmt -> [Eval CondStmt] -> Maybe (Eval Block) -> Validated (Eval Stmt)
                              go (EvalCondStmt _ x) ys mz = 
                                case validStmts s (catMaybes $ [x] ++ fmap (\(EvalCondStmt _ t) -> t) ys ++ maybeToList (fmap (\(EvalBlock _ t) -> t) mz)) of
                                    Left err -> Failure $ [err]
                                    Right mdt -> Success $ EvalStmt s mdt
        CondStmtView (CondStmt _ (TC b)) -> let RN.NoAnn s _ = ann in TC $ fmap (\(EvalBlock _ t) -> EvalCondStmt s t) b
        BlockView (Block stmts) -> 
            let RN.NoAnn s _ = ann
                go xs = case validStmts s (catMaybes $ fmap (\(EvalStmt _ t) -> t) xs) of
                                    Left err -> Failure $ [err]
                                    Right mdt -> Success $ EvalBlock s mdt
                    in TC $ bindValidation (go <$> traverse unTC stmts) id
        HeaderView (Header _ mdt _) -> 
            let RN.NoAnn s _ = ann in TC $ Success $ EvalHeader s mdt
        NoEvalView (FuncDef (TC h) lds (TC b)) p -> 
            let RN.NoAnn s _ = ann 
                go :: Eval Header -> [Eval LocalDef] -> Eval Block -> Validated (Eval FuncDef)
                go (EvalHeader _ (Just x)) _ (EvalBlock _ (Just (Just y))) | x == y = Success $ NoEval s p
                go (EvalHeader sl (Just x)) _ (EvalBlock sr (Just (Just y))) | otherwise = Failure $ [WrongReturnType s (sl, x) (sr, y)]
                go (EvalHeader sl (Just x)) _ (EvalBlock _ (Just Nothing)) = Failure $ [NoReturnInFunctionWithReturnType s (sl, x)]
                go (EvalHeader _ Nothing) _ (EvalBlock _ (Just Nothing)) = Success $ NoEval s p
                go (EvalHeader _ Nothing) _ (EvalBlock sr (Just (Just y))) = Failure $ [ReturnInFunctionWithNoReturnType s (sr, y)]
                go (EvalHeader _ Nothing) _ (EvalBlock _ Nothing) = Success $ NoEval s p
                go (EvalHeader sl (Just x)) _ (EvalBlock _ Nothing) = Failure $ [NoReturnInFunctionWithReturnType s (sl, x)]
                    in TC $ bindValidation (go <$> h <*> traverse unTC lds <*> b) id
        NoEvalView x p -> let RN.NoAnn s _ = ann in TC $ fmap (const $ NoEval s p) $ hfoldMap ((fmap $ const ()).unTC) x
        where validStmts :: SourceSpan -> [Maybe DataType] -> Either Error (Maybe (Maybe DataType))
              validStmts _ [] = Right Nothing
              validStmts _ (t : xs) | all (==t) xs = Right $ Just t
              validStmts s _ = Left $ InconsistentReturnTypes s

typecheck :: Term (T :&&: RN.Ann) i -> [Error]
typecheck x = case (unTC $ cata typecheckAlg x) of
                Failure errs -> errs
                Success _ -> []
