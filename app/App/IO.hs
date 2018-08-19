module App.IO where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import System.IO                    (Handle)

import qualified Data.ByteString.Lazy              as LBS
import qualified HaskellWorks.Data.ByteString.Lazy as LBS
import qualified System.IO                         as IO

openOutputFile :: MonadResource m => FilePath -> Maybe Int -> m (ReleaseKey, Handle)
openOutputFile "-" _ = allocate (return IO.stdout) (const (return ()))
openOutputFile filePath maybeBufferSize = allocate open close
  where open  = do
          handle <- IO.openFile filePath IO.WriteMode
          forM_ maybeBufferSize $ \bufferSize ->
            liftIO $ IO.hSetBuffering handle (IO.BlockBuffering (Just bufferSize))
          return handle
        close = IO.hClose

openInputFile :: FilePath -> IO IO.Handle
openInputFile "-"      = return IO.stdin
openInputFile filePath = IO.openBinaryFile filePath IO.ReadMode

readInputFile :: FilePath -> IO LBS.ByteString
readInputFile "-"      = LBS.hGetContentsChunkedBy chunkSize IO.stdin
readInputFile filePath = IO.openBinaryFile filePath IO.ReadMode >>= LBS.hGetContentsChunkedBy chunkSize

readInputFileClassic :: FilePath -> IO LBS.ByteString
readInputFileClassic "-"      = LBS.hGetContents IO.stdin
readInputFileClassic filePath = IO.openBinaryFile filePath IO.ReadMode >>= LBS.hGetContents

readInputFileChunkedBy :: Int -> FilePath -> IO LBS.ByteString
readInputFileChunkedBy size "-" = LBS.hGetContentsChunkedBy size IO.stdin
readInputFileChunkedBy size filePath = do
  h <- IO.openBinaryFile filePath IO.ReadMode
  LBS.hGetContentsChunkedBy size h

writeOutputFile :: FilePath -> LBS.ByteString -> IO ()
writeOutputFile "-"      bs = LBS.hPut IO.stdout bs
writeOutputFile filePath bs = LBS.writeFile filePath bs

chunkSize :: Int
chunkSize = 4 * 1024
