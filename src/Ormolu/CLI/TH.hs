{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ormolu.CLI.TH where

import "template-haskell" Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Data.Functor.Identity (runIdentity)
import Ormolu.Config (PrinterOpts(..), PrinterOptsTotal)
import Ormolu.CLI (toCLI, toCLIArgument)

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
  
-- PrinterOptsTotal -> String
deriveToStringPoTotal :: Q Exp
deriveToStringPoTotal = do
  ConstructorInfo {constructorVariant=RecordConstructor names} <- reifyConstructor 'PrinterOpts
  
  vars <- mapM (const $ newName "x") names
  let pats = map varP vars
  let [x0,x1,x2,x3,x4,x5,x6,x7,x8] = map varE vars
  
  let names' = ("--" <>) . toCLI . nameBase <$> names
  lamE [conP 'PrinterOpts pats] [|
    let vals = [ toCLIArgument $ runIdentity $x0
               , toCLIArgument $ runIdentity $x1
               , toCLIArgument $ runIdentity $x2
               , toCLIArgument $ runIdentity $x3
               , toCLIArgument $ runIdentity $x4
               , toCLIArgument $ runIdentity $x5
               , toCLIArgument $ runIdentity $x6
               , toCLIArgument $ runIdentity $x7
               , toCLIArgument $ runIdentity $x8 ]
    in zip names' vals
    |]