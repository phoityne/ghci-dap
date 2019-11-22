
module GHCi.DAP where

import Data.Word
import qualified Data.Text as T
import qualified Text.Read as R
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS

-- |
--   Encode DAP Request Argument to [Word8] Show String.
--
encode :: Show a
       => a
       -> String
encode = show . BS.unpack . TE.encodeUtf8 . T.pack . show


-- |
--   Decode [Word8] Show String to DAP Request Argument.
--
decode :: Read a
       => String
       -> Either String a
decode argsStr = case R.readEither argsStr :: Either String [Word8] of
  Left err -> Left $ "read [Word8] failed. " ++ err ++ " : " ++ argsStr
  Right bs -> case R.readEither (toStr bs) of
    Left err -> Left $ "read failed. " ++ err ++ " : " ++  (toStr bs)
    Right a  -> Right a
  where
    toStr = T.unpack . TE.decodeUtf8 . BS.pack

