module Quote (quote) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

quote = QuasiQuoter 
  { quoteExp  = stringE
  , quotePat  = undefined
  , quoteDec  = undefined
  , quoteType = undefined
  }
