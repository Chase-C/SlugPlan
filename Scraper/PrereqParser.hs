{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Scraper.PrereqParser where

import Prelude       hiding (readFile, putStrLn)
import Data.Text.IO  (readFile, putStrLn, putStr)
import Data.Maybe    (mapMaybe, listToMaybe)
import Data.List     (partition)
import Data.Text     (Text, pack)
import Data.Either   (isLeft, lefts, rights)
import Control.Monad (void)

import Text.Parsec         ((<|>))
import Control.Applicative ((<*), (<$>))

import qualified Data.Text       as T
import qualified Text.Parsec     as Parsec

import Scraper.ParserTypes
import Scraper.Subjects

anySubjectName :: UCSCParser String
anySubjectName = Parsec.try $ createMultiParser
                            $ map subjectName [AcadEnglish ..]
                            ++ map snd subjectStrings

parsePrereqs :: Subject -> Text -> Either Parsec.ParseError PrereqTree
parsePrereqs sub input = Parsec.parse (prereqParser sub) "" input

prereqParser :: Subject -> UCSCParser PrereqTree
prereqParser sub = flattenTree
               <$> removeNoPreqreqs
               <$> buildTree '.'
               <$> fillInSubjects sub
               <$> Parsec.many (prereqUnit sub)

countCourses :: Subject -> Text -> Int
countCourses sub input =
    case ps of
        (Right xs) ->
            let cs = filter isCourse xs
                isCourse (PrereqCourse _ _ _) = True
                isCourse _                    = False
            in  length cs
        (Left  _ ) -> -1
    where ps = Parsec.parse (Parsec.many (prereqUnit sub)) "" input

countPreqs :: Either Parsec.ParseError PrereqTree -> Int
countPreqs (Right (Prereq _ _))   = 1
countPreqs (Right (AndPrereq xs)) = sum $ map countPreqs' xs
countPreqs (Right (OrPrereq xs))  = sum $ map countPreqs' xs
countPreqs _              = 0

countPreqs' :: PrereqTree -> Int
countPreqs' (Prereq _ _)   = 1
countPreqs' (AndPrereq xs) = sum $ map countPreqs' xs
countPreqs' (OrPrereq xs)  = sum $ map countPreqs' xs
countPreqs' _              = 0

flattenTree :: PrereqTree -> PrereqTree
flattenTree tree = case tree of
    AndPrereq xs -> let (ys, zs)       = partition isAnd $ map flattenTree xs
                        ands           = concatMap (\(AndPrereq x) -> x) ys
                        (AndPrereq fs) = foldl addToTree (AndPrereq zs) ands
                    in  if length fs == 1
                          then head      fs
                          else AndPrereq fs
    OrPrereq  xs -> let (ys, zs)      = partition isOr $ map flattenTree xs
                        ors           = concatMap (\(OrPrereq x) -> x) ys
                        (OrPrereq fs) = foldl addToTree (OrPrereq zs) ors
                    in  if length fs == 1
                          then head     fs
                          else OrPrereq fs
    t            -> t
    where isAnd (AndPrereq _) = True
          isAnd _             = False
          isOr  (OrPrereq  _) = True
          isOr  _             = False

removeNoPreqreqs :: PrereqTree -> PrereqTree
removeNoPreqreqs tree = case tree of
    AndPrereq xs -> AndPrereq $ map removeNoPreqreqs $ filter f xs
    OrPrereq  xs -> OrPrereq  $ map removeNoPreqreqs $ filter f xs
    x            -> x
    where f (NoPrereq _) = False
          f _            = True

buildTree :: Char -> [PrereqStruct] -> PrereqTree
buildTree _ []     = NoPrereq ""
buildTree _ (x:[]) = case x of
    PrereqCourse (Just s) c t -> AndPrereq [Prereq s c]
    PrereqCourse Nothing  c t -> AndPrereq [NoPrereq c]
    PrereqJunk            c   -> AndPrereq [NoPrereq c]
    PrereqSep             c   -> AndPrereq [NoPrereq [c]]
    PrereqAnd                 -> AndPrereq [NoPrereq "And"]
    PrereqOr                  -> AndPrereq [NoPrereq "Or"]
buildTree lastSep xs
    | null zs   = AndPrereq courses
    | isSep     =
        let sep = getSep zs
        in  buildFromSep sep lastSep (AndPrereq courses) (tail zs)
    | nZs 2     =
        let (a : b : rs) = zs
            sep  = getSep rs
            tree = addToPrereqToken a $ toCourse b : courses
        in  buildFromSep sep lastSep tree rs
    | nZs 3     = NoPrereq "Three Zs"
    | nZs 4     =
        let (a : b : c : d : rs) = zs
            sep = getSep rs
            t1  = addToPrereqToken c $ map toCourse (b : d : [])
            t2  = addToPrereqToken a (t1 : courses)
        in  buildFromSep sep lastSep t2 rs
    | nZs 5     =
        let (a : b : c : d : e : rs) = zs
            sep = getSep rs
            t1  = addToPrereqToken a (toCourse b : courses)
            t2  = case c of
                PrereqSep _ -> addToPrereqToken d (toCourse e : t1 : [])
                _           -> addToPrereqToken a
                                 (NoPrereq (show c) : toCourse b : courses)
        in  buildFromSep sep lastSep t2 rs
    | nYs 1     =
        let (a : b : rs) = zs
        in  case head ys of
            PrereqCourse _ c False -> addToTree (addToPrereqToken a courses)
                                                (buildTree lastSep rs)
            PrereqCourse _ c True  -> addToTree (buildTree lastSep rs)
                                                (addToPrereqToken a (toCourse b:courses))
            _                      -> buildTree lastSep rs
    | otherwise =
        let (a : b : rs) = zs
        in  addToTree (buildTree lastSep rs) (addToPrereqToken a (toCourse b : courses))
    where (ys, zs) = break endSeg $ removeFirstSep xs
          courses  = getCourses ys
          isSep    = isHardSep $ head zs
          nZs n    = length zs == n || isHardSep (zs !! n)
          nYs n    = length ys == n
          buildFromSep sep last tree rest = if stopSep sep last
                then addToTree (buildTree '.' rest) tree
                else addToTree tree (buildTree sep rest)

fillInSubjects :: Subject -> [PrereqStruct] -> [PrereqStruct]
fillInSubjects subject [] = []
fillInSubjects subject (c@(PrereqCourse (Just s) x t) : xs) = c : fillInSubjects s xs
fillInSubjects subject    (PrereqCourse Nothing  x t  : xs) =
    (PrereqCourse (Just subject) x t)  : fillInSubjects subject xs
fillInSubjects subject (x:xs) =
    x  : fillInSubjects subject xs

getSep :: [PrereqStruct] -> Char
getSep [] = '.'
getSep xs = case head xs of
    PrereqSep x   -> x
    _             -> '.'

toCourse :: PrereqStruct -> PrereqTree
toCourse (PrereqCourse (Just s) x t) = Prereq s x
toCourse (PrereqCourse Nothing  x t) = NoPrereq $ "No Subject: " ++ x
toCourse x                           = NoPrereq $ "Not Course: " ++ show x

addToPrereqToken :: PrereqStruct -> [PrereqTree] -> PrereqTree
addToPrereqToken PrereqAnd xs = AndPrereq xs
addToPrereqToken PrereqOr  xs = OrPrereq  xs
addToPrereqToken _         _  = NoPrereq "Not Token"

addToTree :: PrereqTree -> PrereqTree -> PrereqTree
addToTree (AndPrereq xs) x = AndPrereq (x : xs)
addToTree (OrPrereq  xs) x = OrPrereq  (x : xs)
addToTree x _              = x

removeFirstSep :: [PrereqStruct] -> [PrereqStruct]
removeFirstSep ((PrereqSep _):xs) = xs
removeFirstSep xs                 = xs

endSeg :: PrereqStruct -> Bool
endSeg PrereqAnd     = True
endSeg PrereqOr      = True
endSeg (PrereqSep c) = case c of
                           ',' -> False
                           _   -> True
endSeg x             = False

getCourses :: [PrereqStruct] -> [PrereqTree]
getCourses xs = mapMaybe f xs
    where f (PrereqCourse (Just s) x t) = Just $ Prereq s x
          f _                           = Nothing

stopSep :: Char -> Char -> Bool
stopSep ';' '.' = True
stopSep _   _   = False

isHardSep :: PrereqStruct -> Bool
isHardSep (PrereqSep ';') = True
isHardSep (PrereqSep '.') = True
isHardSep _               = False

prereqUnit :: Subject -> UCSCParser PrereqStruct
prereqUnit sub = do
    Parsec.spaces
    struct <- Parsec.try (prereqCourse sub)
        <|> Parsec.try (prereqFlag sub)
        <|> Parsec.try prereqSep
        <|> prereqJunk sub
    Parsec.spaces
    return struct

prereqCourse :: Subject -> UCSCParser PrereqStruct
prereqCourse sub = do
    subStr <- Parsec.optionMaybe subject
    Parsec.spaces
    num <- number
    ret <- Parsec.optionMaybe $ Parsec.lookAhead $ Parsec.oneOf ".,;"
    Parsec.spaces
    return $ case subStr of
        Nothing  -> PrereqCourse Nothing num False
        (Just s) -> let subject = if s == "course" || s == "courses"
                                    then Just sub
                                    else getSubjectFromStringExt s
                    in  PrereqCourse subject num True
    where subject = anySubjectName
                    <|> (Parsec.try $ Parsec.string "courses")
                    <|> (Parsec.try $ Parsec.string "course")
          number  = do
            ds <- Parsec.many1 Parsec.digit
            l  <- Parsec.optionMaybe $ do
                Parsec.notFollowedBy
                    (Parsec.try (Parsec.string "or") <|> Parsec.string "and")
                Parsec.letter
            return $ case l of
                (Just c) -> ds ++ [c]
                Nothing  -> ds

prereqFlag :: Subject -> UCSCParser PrereqStruct
prereqFlag sub = (Parsec.try $ do
    Parsec.string "and"
    Parsec.spaces
    Parsec.lookAhead $ prereqCourse sub
    return PrereqAnd
    ) <|> (do
        Parsec.string "or"
        Parsec.spaces
        Parsec.lookAhead $ prereqCourse sub
        return PrereqOr
        )

prereqSep :: UCSCParser PrereqStruct
prereqSep = do
    c <- Parsec.oneOf ",;."
    Parsec.spaces
    return $ PrereqSep c

prereqJunk :: Subject -> UCSCParser PrereqStruct
prereqJunk sub = do
    f    <- Parsec.anyChar
    junk <- Parsec.manyTill Parsec.anyChar $ Parsec.try $ Parsec.lookAhead nextPrereq
    return $ PrereqJunk (f : junk)
    where nextPrereq = Parsec.try (void $ prereqCourse sub) <|> Parsec.eof


subjectStrings :: [(Subject, String)]
subjectStrings =
    [ (AMS, "Applied Mathematics or Statistics")
    ]

getSubjectFromStringExt :: String -> Maybe Subject
getSubjectFromStringExt strn =
    case listToMaybe search of
        Nothing -> getSubjectFromString strn
        m       -> fst <$> m
    where search = filter (\(_, s) -> (s == strn)) subjectStrings
