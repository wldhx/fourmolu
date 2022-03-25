module Main where

-- | Something.
data Foo = Foo
<<<<<<< HEAD:data/examples/declaration/data/record-bundle=fourmolu-out.hs
    { fooX :: Int
    -- ^ X
    , fooY :: Int
    -- ^ Y
    , fooBar, fooBaz :: NonEmpty (Identity Bool)
    -- ^ BarBaz
    , fooGag
      , fooGog ::
        NonEmpty
            ( Indentity
                Bool
            )
    -- ^ GagGog
    , fooFoo
      , barBar ::
        Int
    -- ^ Huh!
=======
    { -- | X
      fooX :: Int
    , -- | Y
      fooY :: Int
    , -- | BarBaz
      fooBar, fooBaz :: NonEmpty (Identity Bool)
    , -- | GagGog
      fooGag
      , fooGog :: NonEmpty
                    ( Indentity
                        Bool
                    )
    , -- | Huh!
      fooFoo
      , barBar :: Int
>>>>>>> expiplus1/joe-align-leading-arrow:data/examples/declaration/data/record-four-out.hs
    }
    deriving (Eq, Show)
