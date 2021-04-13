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
import qualified Text.Megaparsec.Pos as MP
import Danac.Util.SourceSpan
import Data.Char (isSpace)
import Data.Bifunctor (bimap)
import Data.Text.Conversions (convertText)

toErrataHelper :: T.Text -> T.Text -> SourceSpan -> Errata
toErrataHelper text msg (SS (MP.SourcePos fp l1 c1) (MP.SourcePos _ l2 c2)) =
    errataSimple (Just msg)
        (Block 
            fancyRedStyle 
            (fp, MP.unPos l1, MP.unPos c1) 
            Nothing
            (spanToPointers (MP.unPos l1, MP.unPos c1) (MP.unPos l2, MP.unPos c2) Nothing text)
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
errorRNToErrata text (RN.UndefinedVariable _ sp) = toErrataHelper text "error: undefined variable" sp
errorRNToErrata text (RN.UndefinedFunction _ sp) = toErrataHelper text "error: undefined function" sp
errorRNToErrata text (RN.UndefinedLabel _ sp) = toErrataHelper text "error: undefined label" sp
errorRNToErrata text (RN.AlreadyDefinedVariable _ sp1 sp2) = toErrataHelper' text "error: redefined variable" "redefined here" sp1  "variable originally defined here" sp2
errorRNToErrata text (RN.AlreadyDefinedFunction _ sp Nothing) = toErrataHelper text "error: redefined standard library function" sp
errorRNToErrata text (RN.AlreadyDefinedFunction _ sp1 (Just sp2)) = toErrataHelper' text "error: redefined function" "redefined here" sp1 "function originally defined here" sp2
errorRNToErrata text (RN.AlreadyDefinedLabel _ sp1 sp2) = toErrataHelper' text "error: redefined label" "redefined here" sp1 "label originally defined here" sp2

prettyRNErrors :: T.Text -> [RN.Error] -> LT.Text
prettyRNErrors text = prettyErrors text . fmap (errorRNToErrata text)

errorTCToErrata :: T.Text -> TC.Error -> Errata
errorTCToErrata text (TC.LvalueAxLhsWrongType sp _) =
    toErrataHelper text "error: lvalue is not an array" sp 
errorTCToErrata text (TC.LvalueAxRhsNotIntegral sp _) =
    toErrataHelper text "error: array index is not integral" sp 
errorTCToErrata text (TC.ExprIncompatibleTypes (sp1, dt1) (sp2, dt2)) =
    toErrataHelper' text "error: incompatible expression types" ("parameter is of type " <> convertText (show dt1)) sp1 ("but expression has type " <> convertText (show dt2)) sp2
errorTCToErrata text (TC.ExprProcedureCallNotAllowed sp _) =
    toErrataHelper text "error: can't call a procedure in an expression" sp
errorTCToErrata text (TC.FuncCallWrongNumberOfParameters sp _ _ _) =
    toErrataHelper text "error: wrong number of arguments, expected" sp -- TODO: maybe add new type of helper ?
errorTCToErrata text (TC.FuncCallIncompatibleArg (_, fpt) (sp, dt)) =
    toErrataHelper text ("error: wrong expression passed to argument, expected " <> convertText (show fpt) <> ", got " <> convertText (show dt)) sp
errorTCToErrata text (TC.ReturnPointerType sp _) = 
    toErrataHelper text "error: can't return expression of pointer type" sp
errorTCToErrata text (TC.ReturnArrayType sp _ _) = 
    toErrataHelper text "error: can't return expression of array type" sp
errorTCToErrata text (TC.PointerOnAssignLhs sp) =   
    toErrataHelper text "error: can't assign to array type" sp
errorTCToErrata text (TC.ArrayOnAssignLhs sp) =   
    toErrataHelper text "error: can't assign to array type" sp
errorTCToErrata text (TC.AssignTypeMismatch (sp1, tp1) (sp2, tp2)) =   
    toErrataHelper' text "error: assignment type mismatch" ("lvalue is of type " <> convertText (show tp1)) sp1 ("expression is of type " <> convertText (show tp2)) sp2
errorTCToErrata text (TC.InconsistentReturnTypes (sp1, tp1) (sp2, tp2)) =   
    toErrataHelper' text "error: inconsistent return types" ("return is of type " <> convertText (show tp1)) sp1 ("return is of type " <> convertText (show tp2)) sp2
errorTCToErrata text (TC.ExitInFunctionWithReturnType sp (hsp, _)) =   
    toErrataHelper' text "error: exit used inside function" "exit used here" sp ("return required") hsp
errorTCToErrata text (TC.NoReturnInFunctionWithReturnType (hsp, _)) =   
    toErrataHelper text "error: no return used inside function" hsp
errorTCToErrata text (TC.WrongReturnType _ (sp1, dt1) (sp2, dt2)) = 
    toErrataHelper' text "error: wrong return type" ("required type is " <> convertText (show dt1)) sp1 ("actual type is " <> convertText (show dt2)) sp2
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
        (x, Nothing) -> [go ml (prepareOnly c1 c2) x]
        (x, Just (ms, y)) -> (go Nothing (prepareFirst c1) x : fmap (go Nothing prepareMiddle) ms) ++ [go ml (prepareLast c2) y]
    where go ml' f (l, z) = let (x, y) = f z in Pointer l x y False ml'

