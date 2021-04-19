{-# LANGUAGE OverloadedStrings #-}

module Danac.PrettyErrors
    (prettyRNErrors, 
     prettyTCErrors)
where

import Errata
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Danac.Renamer as RN
import qualified Danac.TypeChecker as TC
import qualified Danac.Ast as A
import qualified Text.Megaparsec.Pos as MP
import Danac.Util.SourceSpan
import Data.Char (isSpace)
import Data.Bifunctor (bimap)
import Data.Text.Conversions (convertText)

toErrataHelper :: T.Text -> T.Text -> Maybe T.Text -> SourceSpan -> Errata
toErrataHelper text msg mt (SS (MP.SourcePos fp l1 c1) (MP.SourcePos _ l2 c2)) =
    errataSimple (Just msg)
        (Block 
            fancyRedStyle 
            (fp, MP.unPos l1, MP.unPos c1) 
            Nothing
            (spanToPointers (MP.unPos l1, MP.unPos c1) (MP.unPos l2, MP.unPos c2) mt text)
            Nothing
        )
        Nothing

toErrataHelper' :: T.Text -> T.Text -> T.Text -> SourceSpan -> T.Text -> SourceSpan -> Errata
toErrataHelper' text msg t1 (SS (MP.SourcePos fp l11 c11) (MP.SourcePos _ l12 c12)) t2 (SS (MP.SourcePos _ l21 c21) (MP.SourcePos _ l22 c22)) =
    errataSimple (Just msg)
        (Block
            fancyRedStyle
            (fp, MP.unPos l11, MP.unPos c11)
            Nothing
            ((spanToPointers (MP.unPos l11, MP.unPos c11) (MP.unPos l12, MP.unPos c12) (Just t1) text) <>
            (spanToPointers (MP.unPos l21, MP.unPos c21) (MP.unPos l22, MP.unPos c22) (Just t2) text))
            Nothing
        )
        Nothing

errorRNToErrata :: T.Text -> RN.Error -> Errata
errorRNToErrata text (RN.UndefinedVariable _ sp) = toErrataHelper text "error: undefined variable" Nothing sp
errorRNToErrata text (RN.UndefinedFunction _ sp) = toErrataHelper text "error: undefined function" Nothing sp
errorRNToErrata text (RN.UndefinedLabel _ sp) = toErrataHelper text "error: undefined label" Nothing sp
errorRNToErrata text (RN.AlreadyDefinedVariable _ sp1 sp2) = toErrataHelper' text "error: redefined variable" "redefined here" sp1  "variable originally defined here" sp2
errorRNToErrata text (RN.AlreadyDefinedFunction _ sp Nothing) = toErrataHelper text "error: redefined standard library function" Nothing sp
errorRNToErrata text (RN.AlreadyDefinedFunction _ sp1 (Just sp2)) = toErrataHelper' text "error: redefined function" "redefined here" sp1 "function originally defined here" sp2
errorRNToErrata text (RN.AlreadyDefinedLabel _ sp1 sp2) = toErrataHelper' text "error: redefined label" "redefined here" sp1 "label originally defined here" sp2

prettyRNErrors :: T.Text -> [RN.Error] -> LT.Text
prettyRNErrors text = prettyErrors text . fmap (errorRNToErrata text)

prettyLvalueType :: TC.LvalueType -> T.Text
prettyLvalueType (TC.LType t) = prettyType t
prettyLvalueType (TC.LPointer t) = prettyType t <> "[]"

prettyValueType (TC.RValue t) = prettyDataType t
prettyValueType (TC.LValue t) = prettyLvalueType t

prettyDataType :: A.DataType -> T.Text
prettyDataType A.Integ = "int"
prettyDataType A.Byte = "byte"

prettyType :: A.Type -> T.Text
prettyType (A.DType t) = prettyDataType t
prettyType (A.AType t i) = prettyType t <> convertText ("["<> show i <> "]")

prettyFparType :: A.FparType -> T.Text
prettyFparType (A.Value t) = prettyType t
prettyFparType (A.Ref t) = prettyDataType t
prettyFparType (A.Pointer t) = prettyType t <> "[]"

errorTCToErrata :: T.Text -> TC.Error -> Errata
errorTCToErrata text (TC.LvalueAxLhsWrongType sp t) =
    toErrataHelper text "error: accessing non-array lvalue" (Just $ "is of type " <> prettyLvalueType t) sp 
errorTCToErrata text (TC.LvalueAxRhsNotIntegral sp t) =
    toErrataHelper text "error: array index is not integral" (Just $ "is of type " <> prettyValueType t) sp 
errorTCToErrata text (TC.ExprIncompatibleTypes (sp1, dt1) (sp2, dt2)) =
    toErrataHelper' text "error: incompatible expression types" ("is of type " <> prettyValueType dt1) sp1 ("is of type " <> prettyValueType dt2) sp2
errorTCToErrata text (TC.ExprProcedureCallNotAllowed sp _) =
    toErrataHelper text "error: can't call a procedure in an expression" (Just "calling a procedure")  sp
errorTCToErrata text (TC.FuncCallWrongNumberOfParameters sp t (A.FunctionType _ p) i) =
    toErrataHelper text "error: wrong number of arguments" (Just $ t <> " expects " <> convertText (show $ length p) <> " parameters, called with " <> convertText (show i)) sp
errorTCToErrata text (TC.FuncCallIncompatibleArg (_, fpt@(A.Value (A.AType _ _))) (sp, vt@(TC.RValue _))) =
    toErrataHelper text "error: wrong argument" (Just $ "expected " <> prettyFparType fpt <> ", got rvalue of type " <> prettyValueType vt) sp
errorTCToErrata text (TC.FuncCallIncompatibleArg (_, fpt@(A.Value (A.AType _ _))) (sp, vt@(TC.LValue _))) =
    toErrataHelper text "error: wrong argument" (Just $ "expected " <> prettyFparType fpt <> ", got lvalue of type " <> prettyValueType vt) sp
errorTCToErrata text (TC.FuncCallIncompatibleArg (_, fpt@(A.Value _)) (sp, vt)) =
    toErrataHelper text "error: wrong argument" (Just $ "expected " <> prettyFparType fpt <> ", got " <> prettyValueType vt) sp
errorTCToErrata text (TC.FuncCallIncompatibleArg (_, fpt@(A.Ref _)) (sp, vt@(TC.RValue _))) =
    toErrataHelper text "error: wrong argument" (Just $ "expected reference of type " <> prettyFparType fpt <> ", got rvalue of type " <> prettyValueType vt) sp
errorTCToErrata text (TC.FuncCallIncompatibleArg (_, fpt@(A.Ref _)) (sp, vt@(TC.LValue _))) =
    toErrataHelper text "error: wrong argument" (Just $ "expected reference of type " <> prettyFparType fpt <> ", got lvalue of type " <> prettyValueType vt) sp
errorTCToErrata text (TC.FuncCallIncompatibleArg (_, fpt@(A.Pointer _)) (sp, vt@(TC.RValue _))) =
    toErrataHelper text "error: wrong argument" (Just $ "expected " <> prettyFparType fpt <> ", got rvalue of " <> prettyValueType vt) sp
errorTCToErrata text (TC.FuncCallIncompatibleArg (_, fpt@(A.Pointer _)) (sp, vt@(TC.LValue _))) =
    toErrataHelper text "error: wrong argument" (Just $ "expected " <> prettyFparType fpt <> ", got lvalue of " <> prettyValueType vt) sp
errorTCToErrata text (TC.ReturnPointerType sp _) = 
    toErrataHelper text "error: can't return expression of pointer type" Nothing sp
errorTCToErrata text (TC.ReturnArrayType sp _ _) = 
    toErrataHelper text "error: can't return expression of array type" Nothing sp
errorTCToErrata text (TC.PointerOnAssignLhs sp t) =   
    toErrataHelper text "error: can't assign to variable of array type" (Just $ "is of type " <> prettyLvalueType (TC.LPointer t)) sp
errorTCToErrata text (TC.ArrayOnAssignLhs sp t i) =   
    toErrataHelper text "error: can't assign to variable of array type" (Just $ "is of type " <> prettyType (A.AType t i)) sp
errorTCToErrata text (TC.AssignTypeMismatch (sp1, tp1) (sp2, tp2)) =   
    toErrataHelper' text "error: assignment type mismatch" ("is of type " <> prettyLvalueType tp1) sp1 ("is of type " <> prettyValueType tp2) sp2
errorTCToErrata _ (TC.InconsistentReturnTypes (_, Nothing) (_, Nothing)) = error "Internal compiler error - inconsistent return type on exits"
errorTCToErrata text (TC.InconsistentReturnTypes (sp1, Nothing) (sp2, Just tp)) =   
    toErrataHelper' text "error: inconsistent return types" "exit has no type" sp1 ("is of type " <> prettyDataType tp) sp2
errorTCToErrata text (TC.InconsistentReturnTypes (sp1, Just tp) (sp2, Nothing)) =   
    toErrataHelper' text "error: inconsistent return types" ("is of type " <> prettyDataType tp) sp1 "exit has no type" sp2
errorTCToErrata text (TC.InconsistentReturnTypes (sp1, Just l) (sp2, Just r)) =   
    toErrataHelper' text "error: inconsistent return types" ("is of type " <> prettyDataType l) sp1 ("is of type " <> prettyDataType r) sp2
errorTCToErrata text (TC.ExitInFunctionWithReturnType sp (hsp, _)) =   
    toErrataHelper' text "error: exit used inside function" "exit used here" sp ("return required") hsp
errorTCToErrata text (TC.NoReturnInFunctionWithReturnType (hsp, t)) =   
    toErrataHelper text "error: no return used inside function" (Just $ "expects a return of type " <> prettyDataType t)  hsp
errorTCToErrata text (TC.WrongReturnType _ (sp1, dt1) (sp2, dt2)) = 
    toErrataHelper' text "error: wrong return type" ("required type is " <> prettyDataType dt1) sp1 ("actual type is " <> prettyDataType dt2) sp2
errorTCToErrata text (TC.ReturnInFunctionWithNoReturnType sp (hsp, _)) =   
    toErrataHelper' text "error: return used in procedure" "return used here" sp ("exit required") hsp

prettyTCErrors :: T.Text -> [TC.Error] -> LT.Text
prettyTCErrors text = prettyErrors text . fmap (errorTCToErrata text)

firstLastNonWs :: T.Text -> (Int, Int)
firstLastNonWs t = let (ws, r) = T.span isSpace t
                       m = T.stripEnd r
                       f = T.length ws + 1
                       l = f + T.length m
                   in (f, l)

prepareFirst :: Int -> T.Text -> (Int, Int)
prepareFirst c = bimap (+(c-1)) (+(c-1)) . firstLastNonWs . T.drop (c-1)

prepareMiddle :: T.Text -> (Int, Int)
prepareMiddle = firstLastNonWs

prepareLast :: Int -> T.Text -> (Int, Int)
prepareLast c = firstLastNonWs . T.take c

prepareOnly :: Int -> Int -> T.Text -> (Int, Int)
prepareOnly c1 c2 = prepareFirst c1 . T.take c2
                        

splitLast :: [a] -> Maybe ([a], a)
splitLast [] = Nothing
splitLast (x:xs) = case splitLast xs of
                        Nothing -> Just ([], x)
                        Just (xs',l) -> Just (x:xs', l)

splitFirstLast :: [a] -> (a, Maybe ([a], a))
splitFirstLast [] = error "Internal compiler error - provided span does not correspond to a valid text region"
splitFirstLast (x:xs) = (x, splitLast xs)

relevantLines :: Int -> Int -> T.Text -> [(Int, T.Text)]
relevantLines x y = take (y-x+1) . drop (x-1) . zip [1..] . T.lines

spanToPointers :: (Int, Int) -> (Int, Int) -> Maybe T.Text -> T.Text -> [Pointer]
spanToPointers (l1, c1) (l2, c2) ml t = 
    case splitFirstLast $ relevantLines l1 l2 t of
        (x, Nothing) -> [go ml (prepareOnly c1 (c2-1)) x]
        (x, Just (ms, y)) -> (go Nothing (prepareFirst c1) x : fmap (go Nothing prepareMiddle) ms) ++ [go ml (prepareLast (c2-1)) y]
    where go ml' f (l, z) = let (x, y) = f z in Pointer l x y False ml'

