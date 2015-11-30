module NGramCrackers.Ops.Infixes
(
(<#>)
) where

import qualified Data.Text as T

{-| Infix synonym for T.append. Handy for gluing bits of Text together -}
(<#>) :: T.Text -> T.Text -> T.Text
(<#>) = T.append

