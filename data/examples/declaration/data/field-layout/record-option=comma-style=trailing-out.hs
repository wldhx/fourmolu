module Main where

-- | Foo.
data Foo = Foo
    { -- | Something
<<<<<<< HEAD:data/examples/declaration/data/field-layout/record-option=comma-style=trailing-out.hs
      foo :: Foo Int Int,
      -- | Something else
      bar ::
        Bar
            Char
            Char
=======
      foo :: Foo Int Int
    , -- | Something else
      bar :: Bar
                Char
                Char
>>>>>>> expiplus1/joe-align-leading-arrow:data/examples/declaration/data/field-layout/record-four-out.hs
    }
