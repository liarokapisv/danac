{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleInstances #-}

module Danac.Util.Derive where

import Data.Comp.Multi.Show
import Data.Comp.Derive.Utils
import Data.Comp.Multi.HFunctor
import Language.Haskell.TH

showConstr :: String -> [String] -> String
showConstr con [] = con
showConstr con args = "(" ++ con ++ " " ++ unwords args ++ ")"

newtype NoQuotes = NoQuotes String
instance Show NoQuotes where show (NoQuotes str) = str

newtype W f = W { unW :: f }

instance Show (W (K String i)) where
    show = unK . unW

instance (Functor f, Show (f NoQuotes)) => Show (W (f (K String i))) where
    show = show . fmap (NoQuotes . unK) . unW

makeShowHF' :: Name -> Q [Dec]
makeShowHF' fname = do
  Just (DataInfo _cxt name args constrs _deriving) <- abstractNewtypeQ $ reify fname
  let args' = init args
      fArg = VarT . tyVarBndrName $ last args'
      argNames = map (VarT . tyVarBndrName) (init args')
      complType = foldl AppT (ConT name) argNames
      preCond = map (mkClassP ''Show . (: [])) argNames
      classType = AppT (ConT ''ShowHF) complType
  constrs' <- mapM normalConExp constrs
  showFDecl <- funD 'showHF (showFClauses fArg constrs')
  return [mkInstanceD preCond classType [showFDecl]]
      where showFClauses fArg = map (genShowFClause fArg)
            filterFarg fArg ty x = (containsType ty fArg, varE x)
            mkShow (isFArg, var)
                | isFArg = [| show (W $var) |]
                | otherwise = [| show $var |]
            genShowFClause fArg (constr, args, ty) = do
              let n = length args
              varNs <- newNames n "x"
              let pat = ConP constr $ map VarP varNs
                  allVars = zipWith (filterFarg (getBinaryFArg fArg ty)) args varNs
                  shows' = listE $ map mkShow allVars
                  conName = nameBase constr
              body <- [|K $ showConstr conName $shows'|]
              return $ Clause [pat] (NormalB body) []
