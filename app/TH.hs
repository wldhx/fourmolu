{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NamedFieldPuns #-}

module TH where

import "template-haskell" Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Data.Aeson ( camelTo2 )
import Ormolu.Config ( PrinterOpts )

poBoolFieldNames :: Q (TExp [String])
poBoolFieldNames = do
  DatatypeInfo{datatypeCons=[c]} <- reifyDatatype ''PrinterOpts
  let RecordConstructor names = constructorVariant c
  let types = [ x | AppT _ (ConT x) <- constructorFields c ]
  
  let fieldNames =
          [ nameBase n |
          (n, t) <- zip names types,
          t == ''Bool ]

  [|| fieldNames ||]
  
toCLI :: String -> String
toCLI = camelTo2 '-' . drop 2