{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.Chunks
  ( cmdChunks
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.Generics.Product.Any
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.ByteString
import HaskellWorks.Data.Vector.AsVector64s
import Options.Applicative                  hiding (columns)

import qualified App.Commands.Options.Type       as Z
import qualified App.IO                          as IO
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.Map                        as M
import qualified HaskellWorks.Simd.Cli.ChunkData as Z
import qualified System.Exit                     as IO
import qualified System.IO                       as IO

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runChunks :: Z.ChunksOptions -> IO ()
runChunks opts = void $ runExceptT $ flip catchError handler $ do
  lbs <- liftIO $ IO.readInputFile (opts ^. the @"inputFile")

  chunkData <- case opts ^. the @"chunkMethod" of
        "chunked"   -> return $ foldMap Z.chunkDataOf (LBS.toChunks lbs)
        "rechunked" -> return $ foldMap Z.chunkDataOf (toByteString <$> asVector64s 64 lbs)
        unknown     -> throwError $ "Unknown method: " <> show unknown

  liftIO $ IO.putStrLn $ "Total chunks: " <> show (chunkData ^. the @"count")
  liftIO $ IO.putStrLn "Chunk histogram: "

  forM_ (chunkData ^. the @"sizes" & M.toList) $ \(size, count) -> do
    liftIO $ IO.putStrLn $ show size <> "," <> show count
  where handler :: String -> ExceptT String IO ()
        handler e = do
          liftIO $ IO.putStrLn $ "Error: " <> e
          void $ liftIO IO.exitFailure


optsChunks :: Parser Z.ChunksOptions
optsChunks = Z.ChunksOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input Text file"
        <>  metavar "STRING"
        )
  <*> strOption
        (   long "chunk-method"
        <>  short 'm'
        <>  help "Chunk method"
        <>  metavar "STRING"
        )

cmdChunks :: Mod CommandFields (IO ())
cmdChunks = command "chunks"  $ flip info idm $ runChunks <$> optsChunks
