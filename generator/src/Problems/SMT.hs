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
import System.Exit

-- combinat
import Math.Combinat.Permutations

-- directory
import System.Directory

-- filepath
import System.FilePath ((</>))

-- lens
import Control.Lens

-- process
import System.Process

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
        over (mapped . _Right) (formatAssert . vary v) template

  forM_ (zip vs [1..]) $ \(v, i :: Int) -> do
    let varied = varyTemplate v
    let content = mconcat
          [ ";; Random number generator seed: ", show optSeed, "\n"
          , ";; Index: ", show i, "\n"
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

        result <- z3 file
        case result of
          Unsat ->
            -- putStrLn "unsat"
            return ()  -- everything fine
          Other s -> do
            putStrLn "Unexpected z3 output:"
            putStrLn s
            exitFailure


-- NOTE: I didn't want to implement a full parser for z3's output,
-- so we just recognize 'unsat' for now
data Z3Result
  = Unsat
  | Other String

z3 :: FilePath -> IO Z3Result
z3 path = do
  output <- readProcess "z3" [path] ""
  if output == "unsat\n"
    then return Unsat
    else return (Other output)


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
vary1 (AddOffset c x) = simplifyAddOffset . substConst c (A "+" [C c, N x])

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


simplifyAddOffset :: Term c -> Term c
simplifyAddOffset = normalizeZero . normalizeNegative . simplifyPlusTwice . normalizePlus

-- | rewrite (- X 3) into (+ X -3)
normalizePlus :: Term c -> Term c
normalizePlus (A "-" [t, N x]) = A "+" [normalizePlus t, N (-x)]
normalizePlus (A fn args) = A fn (map normalizePlus args)
normalizePlus t@(C _) = t
normalizePlus t@(N _) = t

-- | rewrite (+ X -3) into (- X 3)
normalizeNegative :: Term c -> Term c
normalizeNegative (A "+" [t, N x]) | x < 0 = A "-" [normalizeNegative t, N (-x)]
normalizeNegative (A fn args) = A fn (map normalizeNegative args)
normalizeNegative t@(C _) = t
normalizeNegative t@(N _) = t

-- | rewrite (+ X 0) and (- X 0) into X
normalizeZero :: Term c -> Term c
normalizeZero (A "+" [t, N 0]) = normalizeZero t
normalizeZero (A "-" [t, N 0]) = normalizeZero t
normalizeZero (A fn args) = A fn (map normalizeZero args)
normalizeZero t@(C _) = t
normalizeZero t@(N _) = t

-- | rewrite (+ (+ X 3) 5) into (+ X 8)
simplifyPlusTwice :: Term c -> Term c
simplifyPlusTwice (A "+" [A "+" [t, N x], N y]) = A "+" [simplifyPlusTwice t, N (x+y)]
simplifyPlusTwice (A fn args) = A fn (map simplifyPlusTwice args)
simplifyPlusTwice t@(C _) = t
simplifyPlusTwice t@(N _) = t


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


{-
-- 5+c -> c+5
-- c+(-5) -> c-5
normalize1 :: Term String -> Term String
normalize1 (A "+" [ N x, C c ]) = A "+" [ C c, N x ]
normalize1 (A "+" [ C c, N x ]) | x < 0 = A "-" [ C c, N (-x) ]
normalize1 t = t

-- replace c -> c+5
replace :: String -> Int -> Term String -> Term String
replace c x = substC c (A "+" [ C c, N x ])

simplify1 :: Term String -> Term String
simplify1 (A "+" [ N x, N y ]) = N (x + y)
simplify1 (A "+" [A "+" [ A c [], N x ], N y ]) = A "+" [ A c [], N (x + y) ]
-}
