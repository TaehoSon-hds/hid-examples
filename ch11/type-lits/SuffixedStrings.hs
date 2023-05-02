{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SuffixedStrings (SuffixedString, suffixed, asString) where

import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Data.Proxy ( Proxy(..) )

-- Example: suffixed strings

data SuffixedString (suffix :: Symbol) = SS String

suffixed :: String -> SuffixedString suffix
suffixed = SS

asString :: forall suffix. KnownSymbol suffix =>
            SuffixedString suffix -> String
asString (SS str) = str ++ "@" ++ symbolVal (Proxy :: Proxy suffix)
