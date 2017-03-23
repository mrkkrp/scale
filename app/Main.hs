{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import Control.Monad
import Data.CaseInsensitive (CI)
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Yaml.TH hiding (Parser)
import Formatting
import Options.Applicative
import Scale
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict      as M
import qualified Data.Set             as E

-- | Command line options of the application.

data Opts = Opts
  { opTonic   :: Note
    -- ^ Tonic of the scale to print
  , opScale   :: CI String
    -- ^ Name of the scale to print
  }

parserInfo :: ParserInfo Opts
parserInfo = info (helper <*> options)
  (  fullDesc
  <> progDesc "Print a transposition of a scale with interval annotations."
  <> header "scale — a tool to print transpositions of scales" )

options :: Parser Opts
options = Opts
  <$> argument parseNote'
  ( metavar "TONIC" )
  <*> argument (CI.mk <$> str)
  ( metavar "SCALE" )

scales :: Map (CI String) (Set Interval)
scales = M.fromList (f <$> xs)
  where
    xs :: [Scale]
    xs = $$(decodeFile "data/scales.yaml")
    f Scale {..} = (scaleName, scaleIntervals)

main :: IO ()
main = do
  Opts {..} <- execParser parserInfo
  case M.lookup opScale scales of
    Nothing -> putStrLn "I don't know such scale."
    Just scale ->
      forM_ (E.toAscList scale) $ \interval -> do
        let note = shiftNote opTonic interval
        fprint ((right 2 ' ' %. string) % " | " % string % "\n")
          (prettyNote note)
          (prettyInterval interval)

----------------------------------------------------------------------------
-- Helpers

parseNote' :: ReadM Note
parseNote' = eitherReader $ \s ->
  case parseNote s of
    Nothing -> Left ("Could not recognize the note: " ++ s)
    Just x  -> Right x
