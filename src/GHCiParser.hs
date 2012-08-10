{-# LANGUAGE OverloadedStrings #-}
module GHCiParser (
        ensureNoNewLine,
        parseErrors
    ) where

import Control.Arrow
import Data.Aeson
import Data.Aeson.Encode
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L

ensureNoNewLine :: Text -> Text
ensureNoNewLine xs | T.last xs == '\n' = T.init xs
                   | otherwise         = xs

parseErrors :: Text -> Text
parseErrors str = L.toStrict . L.toLazyText . fromValue . toJSON $ [l, c, msg]
  where
    -- <interactive>:1:1:msg.....
    (l, (c, msg)) = second split $ split $ snd (split str)
    split = second (T.drop 1) . T.break ((==) ':')

