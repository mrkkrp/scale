-- |
-- Module      :  Scale
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions of 'Note', 'Interval', and 'Scale'.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Scale
  ( Note (..)
  , prettyNote
  , parseNote
  , Interval (..)
  , prettyInterval
  , parseInterval
  , shiftNote
  , Scale (..) )
where

import Data.Aeson
import Data.CaseInsensitive (CI)
import Data.Char (toLower)
import Data.Data (Data)
import Data.Set (Set)
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import qualified Data.CaseInsensitive as CI
import qualified Data.Set             as E
import qualified Data.Text            as T

-- | Notes (12 tone temperament).

data Note
  = C
  | Cs
  | D
  | Ds
  | E
  | F
  | Fs
  | G
  | Gs
  | A
  | As
  | B
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)

instance Lift Note

-- | Pretty-print a note.

prettyNote :: Note -> String
prettyNote = \case
  C  -> "c"
  Cs -> "c#"
  D  -> "d"
  Ds -> "d#"
  E  -> "e"
  F  -> "f"
  Fs -> "f#"
  G  -> "g"
  Gs -> "g#"
  A  -> "a"
  As -> "a#"
  B  -> "b"

-- | Parse a note.

parseNote :: String -> Maybe Note
parseNote = (\case
  "c"  -> pure C
  "c#" -> pure Cs
  "cs" -> pure Cs
  "d"  -> pure D
  "d#" -> pure Ds
  "ds" -> pure Ds
  "e"  -> pure E
  "f"  -> pure F
  "f#" -> pure Fs
  "fs" -> pure Fs
  "g"  -> pure G
  "g#" -> pure Gs
  "gs" -> pure Gs
  "a"  -> pure A
  "a#" -> pure As
  "as" -> pure As
  "b"  -> pure B
  _    -> Nothing) . normalize

-- | Intervals.

data Interval
  = PerfectUnison -- ^ 0
  | MinorSecond   -- ^ 1 (semitone)
  | MajorSecond   -- ^ 2 (tone)
  | MinorThird    -- ^ 3
  | MajorThird    -- ^ 4
  | PerfectFourth -- ^ 5
  | Tritone       -- ^ 6
  | PerfectFifth  -- ^ 7
  | MinorSixth    -- ^ 8
  | MajorSixth    -- ^ 9
  | MinorSeventh  -- ^ 10
  | MajorSeventh  -- ^ 11
  | PerfectOctave -- ^ 12
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)

instance Lift Interval

instance FromJSON Interval where
  parseJSON = withText "Interval" $ \t ->
    let t' = T.unpack t
    in case parseInterval t' of
        Nothing -> fail ("could not parse interval: " ++ t')
        Just x  -> return x

-- | Pretty-print an interval.

prettyInterval :: Interval -> String
prettyInterval = \case
  PerfectUnison -> "perfect unison"
  MinorSecond   -> "minor second"
  MajorSecond   -> "major second"
  MinorThird    -> "minor third"
  MajorThird    -> "major third"
  PerfectFourth -> "perfect fourth"
  Tritone       -> "tritone"
  PerfectFifth  -> "perfect fifth"
  MinorSixth    -> "minor sixth"
  MajorSixth    -> "major sixth"
  MinorSeventh  -> "minor seventh"
  MajorSeventh  -> "major seventh"
  PerfectOctave -> "perfect octave"

-- | Parse an interval.

parseInterval :: String -> Maybe Interval
parseInterval = (\case
  "perfect unison" -> pure PerfectUnison
  "unison"         -> pure PerfectUnison
  "minor second"   -> pure MinorSecond
  "semitone"       -> pure MinorSecond
  "major second"   -> pure MajorSecond
  "tone"           -> pure MajorSecond
  "minor third"    -> pure MinorThird
  "major third"    -> pure MajorThird
  "perfect fourth" -> pure PerfectFourth
  "fourth"         -> pure PerfectFourth
  "tritone"        -> pure Tritone
  "perfect fifth"  -> pure PerfectFifth
  "fifth"          -> pure PerfectFifth
  "minor sixth"    -> pure MinorSixth
  "major sixth"    -> pure MajorSixth
  "minor seventh"  -> pure MinorSeventh
  "major seventh"  -> pure MajorSeventh
  "perfect octave" -> pure PerfectOctave
  "octave"         -> pure PerfectOctave
  _                -> Nothing) . normalize

-- | Shift a note by a given interval.

shiftNote :: Note -> Interval -> Note
shiftNote n i = toEnum $ (fromEnum n + fromEnum i) `mod` n'
  where
    n' = fromEnum (maxBound :: Note) + 1

-- | A scale.

data Scale = Scale
  { scaleName      :: CI String    -- ^ Scale name
  , scaleIntervals :: Set Interval -- ^ Scale intervals
  } deriving (Eq, Ord, Show, Read, Data, Generic)

instance Lift Scale

instance FromJSON Scale where
  parseJSON = withObject "Scale" $ \o -> do
    scaleName      <- CI.mk      <$> (o .: "name")
    scaleIntervals <- E.fromList <$> (o .: "intervals")
    return Scale {..}

----------------------------------------------------------------------------
-- Helpers

-- | Normalize 'String' with respect to case and white space.

normalize :: String -> String
normalize = fmap toLower . unwords . words
