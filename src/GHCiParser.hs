{-# LANGUAGE OverloadedStrings #-}
module GHCiParser (
        ensureNoNewLine,
        parseErrors,
        jsonText
    ) where

import Control.Arrow
import Data.Aeson
import Data.Aeson.Encode
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L

ensureNoNewLine :: Text -> Text
ensureNoNewLine xs | T.null xs         = T.singleton '\n'
                   | T.last xs == '\n' = T.init xs
                   | otherwise         = xs

parseErrors :: Text -> Text
parseErrors str =
    L.toStrict . L.toLazyText . fromValue . toJSON $ [l, c, T.strip msg]
  where
    -- <interactive>:1:1:msg.....
    (l, c, msg) = if T.null m
                        then ("", "", l' `T.append` c')
                        else (l', c', m)
    (l', (c', m)) = second split $ split $ snd (split str)
    split = second (T.drop 1) . T.break ((==) ':')

jsonText :: Text -> Text
jsonText = L.toStrict . L.toLazyText . fromValue . toJSON

