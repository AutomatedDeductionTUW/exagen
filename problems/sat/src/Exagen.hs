{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Exagen where

-- base
import Control.Monad
import Data.Foldable

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- prettyprinter
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

-- QuickCheck
import Test.QuickCheck

-- exagen
import Logic.Propositional.Formula


main :: IO ()
main = do
  fms <- replicateM 10 generateFormula
  forM_ fms $ \fm -> do
    putStrLn $ showPretty fm
    -- putStrLn $ "Proper Subformulas:\n" <> showPretty (properSubformulas fm)
    putStrLn $ "Polarities: " <> show (atomPolarity fm)
    putStrLn ""


generateFormula :: IO (Formula Prop)
generateFormula = go 1000
  where
    genProp = do
      str <- elements ["p", "q", "r"]
      return (P str)

    numConnectives = 7

    go :: Int -> IO (Formula Prop)
    go 0 = error "max tries exceeded"
    go n = do
      fm <- generate (resize numConnectives $ genFormula genProp)
      if suitable fm
        then return fm
        else go (n - 1)

    suitable :: Ord a => Formula a -> Bool
    suitable fm =
      foldl1 (&&)
      [ numAtoms fm == 3
      , -- there is at least one atom with pure polarity
        hasAtomPolarity Neg fm || hasAtomPolarity Pos fm
      , -- at least one but at most two equivalences
        let n = countSubformulas isIff fm
        in 1 <= n && n <= 2
      , anySubformula isImp fm
      , anySubformula isNot fm
      , anySubformula isAnd fm || anySubformula isOr fm
      , not (anySubformula isNestedNot fm)
      , not (anySubformula isTrivialBinaryConnective fm)
      ]


-- Return true for binary connectives where both arguments are a literal using the same atom.
-- For example, things like (p /\ ~p), (p \/ p), ...
isTrivialBinaryConnective :: Eq a => Formula a -> Bool
isTrivialBinaryConnective = isTrivial
  where
    isTrivial (And f g) = isLiteralWithSameAtom f g
    isTrivial (Or  f g) = isLiteralWithSameAtom f g
    isTrivial (Imp f g) = isLiteralWithSameAtom f g
    isTrivial (Iff f g) = isLiteralWithSameAtom f g
    isTrivial _ = False

    isLiteralWithSameAtom f g =
      case (getAtom f, getAtom g) of
        (Just x, Just y) -> x == y
        _ -> False

    getAtom :: Formula a -> Maybe a
    getAtom (Atomic x) = Just x
    getAtom (Not (Atomic x)) = Just x
    getAtom _ = Nothing


anySubformula :: (Formula a -> Bool) -> Formula a -> Bool
anySubformula p = any p . subformulas

countSubformulas :: (Formula a -> Bool) -> Formula a -> Int
countSubformulas p = length . filter p . subformulas

isNestedNot :: Formula a -> Bool
isNestedNot (Not (Not _)) = True
isNestedNot _ = False

isNot :: Formula a -> Bool
isNot (Not _) = True
isNot _ = False

isAnd :: Formula a -> Bool
isAnd (And _ _) = True
isAnd _ = False

isOr :: Formula a -> Bool
isOr (Or _ _) = True
isOr _ = False

isImp :: Formula a -> Bool
isImp (Imp _ _) = True
isImp _ = False

isIff :: Formula a -> Bool
isIff (Iff _ _) = True
isIff _ = False

-- Number of different atoms that occur in the formula
numAtoms :: Ord a => Formula a -> Int
numAtoms = Set.size . Set.fromList . toList

-- numImp :: Formula a -> Int
-- numImp (Atomic _) = 0
-- numImp (Not f) = numImp f
-- numImp (And f g) = numImp f + numImp g
-- numImp (Or  f g) = numImp f + numImp g
-- numImp (Imp f g) = 1 + numImp f + numImp g
-- numImp (Iff f g) = numImp f + numImp g

numIff :: Formula a -> Int
numIff (Atomic _) = 0
numIff (Not f) = numIff f
numIff (And f g) = numIff f + numIff g
numIff (Or  f g) = numIff f + numIff g
numIff (Imp f g) = numIff f + numIff g
numIff (Iff f g) = 1 + numIff f + numIff g


subformulas :: Formula a -> [Formula a]
subformulas f = f : properSubformulas f

properSubformulas :: Formula a -> [Formula a]
properSubformulas (Atomic _) = []
properSubformulas (Not g) = subformulas g
properSubformulas (And f g) = subformulas f ++ subformulas g
properSubformulas (Or  f g) = subformulas f ++ subformulas g
properSubformulas (Iff f g) = subformulas f ++ subformulas g
properSubformulas (Imp f g) = subformulas f ++ subformulas g


showPretty :: Pretty a => a -> String
showPretty = renderString . layoutPretty layoutOptions . align . pretty
  where
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 80 1 }


hasAtomPolarity :: Ord a => Polarity -> Formula a -> Bool
hasAtomPolarity pol = (pol `elem`) . map snd . Map.toList . atomPolarity



data Polarity
  = Pos
  | Neg
  | Both
  deriving (Eq, Show)

instance Semigroup Polarity where
  Pos <> Pos = Pos
  Neg <> Neg = Neg
  _ <> _ = Both

flipPolarity :: Polarity -> Polarity
flipPolarity Pos = Neg
flipPolarity Neg = Pos
flipPolarity Both = Both

-- Annotate each atom occurrence with its polarity
polarity :: Formula a -> Formula (a, Polarity)
polarity = go Pos
  where
    go pol (Atomic x) = Atomic (x, pol)
    go pol (Not f) = Not (go (flipPolarity pol) f)
    go pol (And f g) = And (go pol f) (go pol g)
    go pol (Or  f g) = Or  (go pol f) (go pol g)
    go pol (Imp f g) = Imp (go (flipPolarity pol) f) (go pol g)
    go _   (Iff f g) = Iff (go Both f) (go Both g)

atomPolarity :: Ord a => Formula a -> Map a Polarity
atomPolarity = Map.fromListWith (<>) . toList . polarity
