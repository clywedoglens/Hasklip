{-# LANGUAGE TypeApplications #-}
module Hasklip.Parser where

import Hasklip.Types (Manifest, ParseResult, ArbeitResult, ParseError(..))

import Data.Aeson (FromJSON)
import Data.Yaml (decodeEither')
import qualified Data.ByteString as BS
import System.IO
import GHC.IO.Handle.FD (openFileBlocking)
import Control.Exception (try, IOException)
import qualified Data.Text as T 

readAndDecode :: FromJSON a => FilePath -> IO (ParseResult a)
readAndDecode path = do
  result <- try @IOException (BS.readFile path)
  case result of
      Left ioException ->
          return $ Left (FileError (T.pack $ show ioException))
      Right content ->
          return $ case decodeEither' content of
              Left err  -> Left (ParserError (T.pack $ show err))
              Right val -> Right val

readManifest :: FilePath -> IO (ParseResult Manifest)
readManifest = readAndDecode

readClipArbeitResults :: IO (ParseResult [ArbeitResult])
readClipArbeitResults = do
    handle <- openFileBlocking "/tmp/cliparbeitresults.fifo" ReadMode
    -- This ensures blocking read
    content <- BS.hGetContents handle
    hClose handle
    return $ case decodeEither' content of
        Left err -> Left (ParserError (T.pack $ show err))
        Right arbeit -> Right arbeit
