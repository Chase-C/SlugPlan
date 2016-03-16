{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Scraper.ParserTypes where

import Prelude
import Model
import Data.Text       (Text)
import Text.Parsec     (Parsec)
import Data.Map.Strict (Map)
import Data.List       (sort, group)

import qualified Text.Parsec as Parsec

import Scraper.Subjects

type SubjectMap   = Map Subject [Course]
type UCSCParser a = Parsec Text () a

data Quarter = Fall | Winter | Spring | Summer
    deriving (Show, Eq, Ord, Enum)

data PrereqTree = AndPrereq  [PrereqTree]
                | OrPrereq   [PrereqTree]
                | Prereq     Subject String
                | NoPrereq   String
                deriving (Show, Eq)

data PrereqStruct = PrereqCourse (Maybe Subject) String Bool
                  | PrereqJunk   String
                  | PrereqSep    Char
                  | PrereqAnd
                  | PrereqOr
                  deriving (Show, Eq)

createMultiParser :: [String] -> UCSCParser String
createMultiParser [] = return ""
createMultiParser strns
    | flen /= slen        = Parsec.option "" $ Parsec.try choices
    | length prefixes > 1 = choices
    | otherwise           = subPar $ head prefixes
    where filtered = sort $ filter (not . null) strns
          prefixes = map head $ group $ map head filtered
          flen     = length filtered
          slen     = length strns
          choices  = Parsec.choice $ map subPar prefixes
          subPar ' ' = do
            Parsec.spaces
            out <- createMultiParser $ map tail $ filter ((==) ' ' . head) filtered
            return $ ' ' : out
          subPar c   = do
            Parsec.char c
            out <- createMultiParser $ map tail $ filter ((==) c . head) filtered
            return $ c : out

