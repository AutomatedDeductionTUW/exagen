{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Problems.SMT where

-- base
import Control.Monad (unless, forM_, ap, join)
import Data.Monoid
import Text.Printf (printf)

-- combinat
import Math.Combinat.Permutations

-- directory
import System.Directory

-- filepath
import System.FilePath ((</>))

-- lens
import Control.Lens

-- random
import System.Random

-- safe
import Safe

-- exagen
import Control.Monad.Choose
import Data.SExpr
import Options (Options(..), SMTOptions(..))


main :: Options -> SMTOptions -> IO ()
main Options{optNumExams,optOutputDir,optSeed} SMTOptions{optTemplate} = do

  putStrLn $ "Reading template from file: " <> optTemplate
  templateStr <- readFile optTemplate
  templateExprs <- do
    case parseSExprs optTemplate templateStr of
      Right t -> return t
      Left err -> fail err

  p <- getStdRandom (randomPermutation (length allVariations))
  let vs = take optNumExams (permuteList p allVariations)

  let template = separate parseAssert templateExprs

      varyTemplate v =
        combine $
        over (mapped . _Right) (formatFormula . vary v) template

  forM_ (zip vs [1..]) $ \(v, i :: Int) -> do
    let varied = varyTemplate v
    let content = mconcat
          [ "% Random number generator seed: ", show optSeed, "\n"
          , "% Index: ", show i, "\n"
          ] ++ unlines (formatExpr id <$> varied)

    case optOutputDir of
      Nothing -> do
        putStrLn ""
        putStrLn content

      Just outputDir -> do
        outputDirExists <- doesDirectoryExist outputDir
        unless outputDirExists $
          error ("The given output directory does not exist: " <> show outputDir)

        let examDir = outputDir </> (printf "exam-%02d" i)
        createDirectoryIfMissing False examDir
        let file = examDir </> "smt.smt2"
        putStrLn $ "Writing file: " <> file
        writeFile file content


variations :: MonadChoose m => m [Variation String]
variations = do
  b_offset <- choose [-3, -2, -1, 0, 1, 2, 3]
  c_offset <- choose [-3, -2, -1, 0, 1, 2, 3]

  return [ AddOffset "b" b_offset
         , AddOffset "c" c_offset
         ]


allVariations :: [[Variation String]]
allVariations = variations @[]



-- | An offset to be added to constant symbols
data Variation c = AddOffset !c !Int
  deriving (Eq, Show)

-- | Apply a single variation to a term
vary1 :: Eq c => Variation c -> Term c -> Term c
vary1 (AddOffset c x) = substConst c (A "+" [C c, N x])

-- | Apply a list of variations
vary :: Eq c => [Variation c] -> Term c -> Term c
vary vs = appEndo (foldMap (Endo . vary1) vs)



separate :: Functor f => (a -> Maybe b) -> f a -> f (Either a b)
separate f = fmap (\x -> maybe (Left x) Right (f x))

combine :: Functor f => f (Either a a) -> f a
combine = fmap fromEither

fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x



data Term c
  = N !Int  -- ^ numeric constant
  | C c     -- ^ uninterpreted constant symbol
  | A !String ![Term c]  -- ^ function application
  deriving (Eq, Ord, Show, Functor, Foldable)


instance Applicative Term where
  pure :: a -> Term a
  pure = C
  (<*>) :: Term (a -> b) -> Term a -> Term b
  (<*>) = ap

-- Monadic bind for terms is substitution of constants by terms
instance Monad Term where
  return :: a -> Term a
  return = pure
  (>>=) :: Term a -> (a -> Term b) -> Term b
  fm >>= f = joinTerm (f <$> fm)

joinTerm :: Term (Term a) -> Term a
joinTerm (C t) = t
joinTerm (N x) = N x
joinTerm (A f ts) = A f (map joinTerm ts)

-- substitute constant c by term t in a term
substConst :: Eq a => a -> Term a -> Term a -> Term a
substConst c t = join . fmap (\d -> if c == d then t else C d)


parseAssert :: Expr String -> Maybe (Term String)
parseAssert (SExpr [Value "assert", formula]) = Just (parseFormula formula)
parseAssert (SExpr (Value "assert" : e)) = error $ "parseAssert: unexpected stuff after assert :" <> show e
parseAssert _ = Nothing  -- declaration or something else


-- NOTE: we just parse the whole formula as a 'term';
-- that's not really correct but good enough for us
parseFormula :: Expr String -> Term String
parseFormula (Value v) =
  case readMay v of
    Just n -> N n
    Nothing -> C v
parseFormula (SExpr (Value fn : args)) = A fn (map parseFormula args)
parseFormula e = error $ "parseFormula: unexpected expression: " <> show e


formatAssert :: Term String -> Expr String
formatAssert formula = SExpr [Value "assert", formatFormula formula]

formatFormula :: Term String -> Expr String
formatFormula (C c) = Value c
formatFormula (N n) = Value (show n)
formatFormula (A fn args) = SExpr (Value fn : map formatFormula args)
