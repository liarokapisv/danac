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

oneLens :: T.Text -> T.Text -> Maybe T.Text -> SourceSpan -> Errata
oneLens text msg mt (SS (MP.SourcePos fp l1 c1) (MP.SourcePos _ l2 c2)) =
    errataSimple (Just msg)
        (Block 
            fancyRedStyle 
            (fp, MP.unPos l1, MP.unPos c1) 
            Nothing
            (spanToPointers False (MP.unPos l1, MP.unPos c1) (MP.unPos l2, MP.unPos c2) mt text)
            Nothing
        )
        Nothing

twoLensesMaybeConnected :: T.Text -> T.Text -> Bool -> Maybe T.Text -> SourceSpan -> Maybe T.Text -> SourceSpan -> Errata
twoLensesMaybeConnected text msg b mt1 (SS (MP.SourcePos fp l11 c11) (MP.SourcePos _ l12 c12)) mt2 (SS (MP.SourcePos _ l21 c21) (MP.SourcePos _ l22 c22)) =
    errataSimple (Just msg)
        (Block
            fancyRedStyle
            (fp, MP.unPos l11, MP.unPos c11)
            Nothing
            ((spanToPointers b (MP.unPos l11, MP.unPos c11) (MP.unPos l12, MP.unPos c12) mt1 text) <>
            (spanToPointers b (MP.unPos l21, MP.unPos c21) (MP.unPos l22, MP.unPos c22) mt2 text))
            Nothing
        )
        Nothing

twoLensesNotConnected text msg = twoLensesMaybeConnected text msg False
twoLensesConnected text msg = twoLensesMaybeConnected text msg True

maybeTwoLensesConnected :: T.Text -> T.Text -> Maybe SourceSpan -> Maybe T.Text -> SourceSpan -> Errata
maybeTwoLensesConnected text msg Nothing mt2 sp2 = oneLens text msg mt2 sp2
maybeTwoLensesConnected text msg (Just (SS (MP.SourcePos fp l11 c11) (MP.SourcePos _ l12 c12))) mt2 (SS (MP.SourcePos _ l21 c21) (MP.SourcePos _ l22 c22)) =
    errataSimple (Just msg)
        (Block
            fancyRedStyle
            (fp, MP.unPos l11, MP.unPos c11)
            Nothing
            ((spanToPointers True (MP.unPos l11, MP.unPos c11) (MP.unPos l12, MP.unPos c12) Nothing text) <>
            (spanToPointers True (MP.unPos l21, MP.unPos c21) (MP.unPos l22, MP.unPos c22) mt2 text))
            Nothing
        )
        Nothing

errorRNToErrata :: T.Text -> RN.Error -> Errata
errorRNToErrata text (RN.UndefinedVariable _ sp) = oneLens text "error: undefined variable" (Just "is undefined") sp
errorRNToErrata text (RN.UndefinedFunction _ sp) = oneLens text "error: undefined function" (Just "is undefined") sp
errorRNToErrata text (RN.UndefinedLabel _ sp) = oneLens text "error: undefined label" (Just "is undefined") sp
errorRNToErrata text (RN.AlreadyDefinedVariable _ sp1 sp2) = twoLensesNotConnected text "error: redefined variable" (Just "redefined here") sp1  (Just "variable originally defined here") sp2
errorRNToErrata text (RN.AlreadyDefinedFunction _ sp Nothing) = oneLens text "error: redefined standard library function" (Just "is a standard library function") sp
errorRNToErrata text (RN.AlreadyDefinedFunction _ sp1 (Just sp2)) = twoLensesNotConnected text "error: redefined function" (Just "redefined here") sp1 (Just "function originally defined here") sp2
errorRNToErrata text (RN.AlreadyDefinedLabel _ sp1 sp2) = twoLensesNotConnected text "error: redefined label" (Just "redefined here") sp1 (Just "label originally defined here") sp2
errorRNToErrata text (RN.BreakUsedOutOfLoop sp) = oneLens text "error: invalid break" (Just "break used outside of loop") sp
errorRNToErrata text (RN.ContinueUsedOutOfLoop sp) = oneLens text "error: invalid continue" (Just "continue used outside of loop") sp

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
    oneLens text "error: invalid access" (Just $ "type " <> prettyLvalueType t <> " is not accessible") sp 
errorTCToErrata text (TC.LvalueAxRhsNotIntegral sp t) =
    oneLens text "error: invalid indexing" (Just $ "is of type " <> prettyValueType t <> ", should be int") sp 
errorTCToErrata text (TC.ExprIncompatibleTypes (sp1, dt1) (sp2, dt2)) =
    twoLensesNotConnected text "error: incompatible expressions" (Just $ "is of type " <> prettyValueType dt1) sp1 (Just $ "is of type " <> prettyValueType dt2) sp2
errorTCToErrata text (TC.ExprProcedureCallNotAllowed sp _) =
    oneLens text "error: invalid procedure call" (Just "calling a procedure in an expression")  sp
errorTCToErrata text (TC.FuncCallWrongNumberOfParameters sp (A.FunctionType _ p) i mhs) =
    maybeTwoLensesConnected text "error: wrong number of arguments" mhs (Just $ " expected " <> convertText (show $ length p) <> " parameters, called with " <> convertText (show i)) sp
errorTCToErrata text (TC.FuncCallIncompatibleArg (_, fpt@(A.Ref _)) (sp, vt@(TC.RValue _)) mhs) =
    maybeTwoLensesConnected text "error: wrong argument" mhs (Just $ "expected reference of type " <> prettyFparType fpt <> ", got rvalue of type " <> prettyValueType vt) sp
errorTCToErrata text (TC.FuncCallIncompatibleArg (_, fpt@(A.Ref _)) (sp, vt@(TC.LValue _)) mhs) =
    maybeTwoLensesConnected text "error: wrong argument" mhs (Just $ "expected reference of type " <> prettyFparType fpt <> ", got lvalue of type " <> prettyValueType vt) sp
errorTCToErrata text (TC.FuncCallIncompatibleArg (_, fpt) (sp, vt) mhs) =
    maybeTwoLensesConnected text "error: wrong argument" mhs (Just $ "expected " <> prettyFparType fpt <> ", got " <> prettyValueType vt) sp
errorTCToErrata text (TC.PointerOnAssignLhs sp t) =   
    oneLens text "error: invalid assignment" (Just $ "can't assign to variable of type " <> prettyLvalueType (TC.LPointer t)) sp
errorTCToErrata text (TC.ArrayOnAssignLhs sp t i) =   
    oneLens text "error: invalid assignment" (Just $ "can't assign to variable of type " <> prettyType (A.AType t i)) sp
errorTCToErrata text (TC.AssignTypeMismatch (sp1, tp1) (sp2, tp2)) =   
    twoLensesNotConnected text "error: assignment type mismatch" (Just $ "is of type " <> prettyLvalueType tp1) sp1 (Just $ "is of type " <> prettyValueType tp2) sp2
errorTCToErrata text (TC.ReturnPointerType (hs, rt) (sp, pt)) = 
    twoLensesConnected text "error: wrong return type" (Just $ "expected return of type" <> prettyDataType rt) hs (Just $  "is of type " <> prettyLvalueType (TC.LPointer pt)) sp
errorTCToErrata text (TC.ReturnArrayType (hs, rt) (sp, at, i)) = 
    twoLensesConnected text "error: wrong return type" (Just $ "expected return of type " <> prettyDataType rt) hs (Just $ "is of type " <> prettyType (A.AType at i)) sp
errorTCToErrata text (TC.ExitInFunctionWithReturnType (hs, rt) sp) =
    twoLensesConnected text "error: invalid exit" (Just $ "expected return of type " <> prettyDataType rt) hs (Just $ "got exit") sp
errorTCToErrata text (TC.NoReturnInFunctionWithReturnType (hs, rt)) =   
    oneLens text "error: missing return" (Just $ "expected return of type " <> prettyDataType rt <> " but none was found") hs
errorTCToErrata text (TC.WrongReturnType (hs, rt) (sp2, dt2)) = 
    twoLensesConnected text "error: wrong return type" (Just $ "expected return of type " <> prettyDataType rt) hs (Just $ "is of type " <> prettyDataType dt2) sp2
errorTCToErrata text (TC.ReturnInFunctionWithNoReturnType hs (sp, vt)) =   
    twoLensesConnected text "error: invalid return" (Just "expected exit") hs (Just $ "got return of type " <> prettyValueType vt) sp

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

spanToPointers :: Bool -> (Int, Int) -> (Int, Int) -> Maybe T.Text -> T.Text -> [Pointer]
spanToPointers b (l1, c1) (l2, c2) ml t = 
    case splitFirstLast $ relevantLines l1 l2 t of
        (x, Nothing) -> [go ml (prepareOnly c1 (c2-1)) x]
        (x, Just (ms, y)) -> (go Nothing (prepareFirst c1) x : fmap (go Nothing prepareMiddle) ms) ++ [go ml (prepareLast (c2-1)) y]
    where go ml' f (l, z) = let (x, y) = f z in Pointer l x y b ml'

