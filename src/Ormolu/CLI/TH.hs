{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ormolu.CLI.TH where

import Control.Monad
import "template-haskell" Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Ormolu.Config (PrinterOpts(..), PrinterOptsTotal)

poBoolFieldNames :: Q (TExp [String])
poBoolFieldNames = do
  DatatypeInfo{datatypeCons=[c]} <- reifyDatatype ''PrinterOpts -- XXX: reifyConstructor
  let RecordConstructor names = constructorVariant c
  let types = [ x | AppT _ (ConT x) <- constructorFields c ]
  
  let fieldNames =
          [ nameBase n |
          (n, t) <- zip names types,
          t == ''Bool ]

  [|| fieldNames ||]
  
deriveToStringPo :: Q Exp
deriveToStringPo = do
  DatatypeInfo{datatypeCons=[c]} <- reifyDatatype ''PrinterOpts
  let RecordConstructor names = constructorVariant c
  let types = [ x | AppT _ (ConT x) <- constructorFields c ]
  let cf = head $ constructorFields c
  
  -- names newName  wildP
  -- types  bool     _
  pats <- let mkPat ty
                  | ty == ''Bool = do
                      n <- newName "x"
                      varP n
                  | otherwise = wildP
    in mapM mkPat types
  
  x <- newName "x"
  lamE [conP 'PrinterOpts [varP x]] (varE x)
  -- RecP Name [FieldPat] ?