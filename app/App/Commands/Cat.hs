{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.Cat
  ( cmdCat
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.Maybe                (fromMaybe)
import Data.Semigroup            ((<>))
import Options.Applicative       hiding (columns)

import qualified App.Commands.Options.Type         as Z
import qualified App.IO                            as IO
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.ByteString.Lazy.Internal     as BSI
import qualified HaskellWorks.Data.ByteString.Lazy as LBS
import qualified System.Exit                       as IO
import qualified System.IO                         as IO

runCat :: Z.CatOptions -> IO ()
runCat opts = case opts ^. the @"method" of
  "default" -> do
    bs <- LBS.hGetContents =<< IO.openInputFile (opts ^. the @"inputFile")
    IO.writeOutputFile (opts ^. the @"outputFile") bs
  "rechunk" -> case opts ^. the @"chunkSize" of
    Just chunkSize -> do
      bs <- LBS.hGetContents =<< IO.openInputFile (opts ^. the @"inputFile")
      IO.writeOutputFile (opts ^. the @"outputFile") (LBS.rechunk chunkSize bs)
    Nothing -> do
      IO.hPutStrLn IO.stderr "Missing option --chunk-size"
      IO.exitWith $ IO.ExitFailure 1
  "resegment" -> case opts ^. the @"chunkSize" of
    Just chunkSize -> do
      bs <- LBS.hGetContents =<< IO.openInputFile (opts ^. the @"inputFile")
      IO.writeOutputFile (opts ^. the @"outputFile") (LBS.resegment chunkSize bs)
    Nothing -> do
      IO.hPutStrLn IO.stderr "Missing option --chunk-size"
      IO.exitWith $ IO.ExitFailure 1
  "prechunk" -> do
    let defaultChunkSize = (BSI.defaultChunkSize `div` 64) * 64
    let chunkSize = fromMaybe defaultChunkSize (opts ^. the @"chunkSize")
    bs <- LBS.hGetContentsChunkedBy chunkSize =<< IO.openInputFile (opts ^. the @"inputFile")
    IO.writeOutputFile (opts ^. the @"outputFile") (LBS.toLazyByteString bs)
  method -> do
    IO.putStrLn $ "Invalid method: " <> method
    IO.exitWith $ IO.ExitFailure 1

optsCat :: Parser Z.CatOptions
optsCat = Z.CatOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input Text file"
        <>  metavar "STRING"
        )
  <*> strOption
        (   long "output"
        <>  short 'o'
        <>  help "Output Text file"
        <>  metavar "STRING"
        )
  <*> strOption
        (   long "method"
        <>  short 'm'
        <>  help "Method"
        <>  metavar "STRING"
        )
  <*> optional (option auto
        (   long "chunk-size"
        <>  short 'c'
        <>  help "Chunk size.  Recommended chunk-size is 32256"
        <>  metavar "BYTES"
        )
      )

cmdCat :: Mod CommandFields (IO ())
cmdCat = command "cat"  $ flip info idm $ runCat <$> optsCat
