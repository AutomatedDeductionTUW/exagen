module Util
  ( module Util  -- export everything in the current module
  , module Debug.Trace  -- re-export everything in Debug.Trace
  , putDoc
  ) where

-- base
import Control.Exception
import Debug.Trace
import Text.Printf (printf)

-- directory
import System.Directory

-- filepath
import System.FilePath

-- list-transformer
import List.Transformer as ListT

-- prettyprinter
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)


assertM :: Applicative m => Bool -> m ()
assertM b = assert b (pure ())


showPretty :: Pretty a => a -> String
showPretty = renderString . layoutPretty defaultLayoutOptions . align . pretty


printPretty :: Pretty a => a -> IO ()
printPretty x = putDocLn (pretty x)


putDocLn :: Doc ann -> IO ()
putDocLn doc = putDoc doc >> putStrLn ""


-- | Collect stream into list.
-- Note that this builds the whole list in memory before returning.
collectStream :: Monad m => ListT m a -> m [a]
collectStream = ListT.fold (flip (:)) [] reverse


-- | Get directory where exam data should be saved,
-- and makes sure that it exists
getExamDir :: FilePath -> Int -> IO FilePath
getExamDir outputDir i = do
  let examDir = outputDir </> (printf "exam-%02d" i)
  createDirectoryIfMissing False examDir
  return examDir
