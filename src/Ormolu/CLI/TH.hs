{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ormolu.CLI.TH where

import "template-haskell" Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Data.Functor.Identity (runIdentity)
import Ormolu.Config (PrinterOpts(..), PrinterOptsTotal, defaultPrinterOpts)
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
    
poFieldNames :: Q (TExp [String])
poFieldNames = do
  ConstructorInfo {constructorVariant=RecordConstructor names} <- reifyConstructor 'PrinterOpts
  let names' = ("--" <>) . toCLI . nameBase <$> names
  [|| names' ||]

showDiffPoG :: Q Exp
showDiffPoG = do
  ConstructorInfo {constructorFields} <- reifyConstructor 'PrinterOpts

  vars <- mapM (const $ newName "x") constructorFields
  let xs_ = map varP vars
  let xs = map varE vars

  defaultVars <- mapM (const $ newName "dx") constructorFields
  let ds_ = map varP defaultVars
  let ds = map varE defaultVars
          
  compVars <- mapM (const $ newName "x_dx") constructorFields
  let cs_ = map varP compVars
  let cs = map varE compVars
  
  let v_cs = map
        (\(c_, x, d) -> valD c_ (normalB [|
          if $x /= $d
            then Just . show . runIdentity $ $x
            else mempty
        |]) [])
        (zip3 cs_ xs ds)
        
  let zzz = foldr
        (\x y -> [| $x : $y |])
        [| [] |]
        cs

  lamE [conP 'PrinterOpts xs_]
       (letE
         (valD
           (conP 'PrinterOpts ds_) (normalB [|defaultPrinterOpts|]) []
           : v_cs)
         [| $zzz |])
  