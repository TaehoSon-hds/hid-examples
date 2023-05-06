module Str where

import Language.Haskell.TH ( stringE )
import Language.Haskell.TH.Quote ( QuasiQuoter(QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType) )

str = QuasiQuoter {
          quoteExp = stringE
        , quotePat = undefined
        , quoteType = undefined
        , quoteDec = undefined
        }
