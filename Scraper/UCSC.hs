{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Scraper.UCSC where

import Model
import Prelude              hiding (readFile, putStrLn)
import Pdf.Toolbox.Document hiding (isLeft)
import Data.Text.IO  (readFile, putStrLn, putStr)
import Data.Maybe    (catMaybes, mapMaybe)
import Data.Char     (toUpper)
import Data.List     (sort, group, partition)
import Data.Text     (Text, unpack, pack)
import Data.Either   (isLeft, lefts, rights)
import Control.Monad (void)

import Text.Parsec         ((<|>))
import Control.Applicative ((<*), (<$>))
import Control.Monad       (when)

import qualified Data.Text       as T
import qualified Text.Parsec     as Parsec
import qualified Data.Map.Strict as Map

import Scraper.ParserTypes
import Scraper.Subjects
import Scraper.PrereqParser

getSubjectMap :: FilePath -> IO SubjectMap
getSubjectMap filename = do
    input <- readFile filename
    let classes = getSubjectMap' input filename
    case classes of
        (Just cs) -> return cs
        _         -> return Map.empty

getSubjectMap' :: Text -> String -> Maybe SubjectMap
getSubjectMap' input filename =
    let classes = Parsec.parse getAllSubjectCourses filename input
    in case classes of
        (Right cs) -> Just cs
        _          -> Nothing


getAllSubjectCourses :: UCSCParser SubjectMap
getAllSubjectCourses = Map.fromList <$> (Parsec.many $ Parsec.try getNextSubjectCourses)

getNextSubjectCourses :: UCSCParser (Subject, [Course])
getNextSubjectCourses = (Parsec.try $ do
    subject <- subjectBegin
    courses <- Parsec.manyTill (getCourse subject) (Parsec.try subjectEnd)
    return (subject, courses)
    ) <|> (Parsec.anyChar >> getNextSubjectCourses)

anySubjectHeader :: UCSCParser String
anySubjectHeader = Parsec.try $ createMultiParser $
    map ((map toUpper) . subjectName) [AcadEnglish ..]

subjectBegin :: UCSCParser Subject
subjectBegin = do
    header <- anySubjectHeader
    Parsec.spaces
    Parsec.optional $ Parsec.string "PROGRAM"
    Parsec.spaces
    Parsec.optional $ Parsec.string "COURSES"
    Parsec.spaces
    courseHeader
    return $ fst $ head $ filter (\(_, s) -> s == header) subjects
    where subjects = map (\s -> (s, map toUpper $ subjectName s)) [AcadEnglish ..]

subjectEnd :: UCSCParser ()
subjectEnd = do
    Parsec.spaces
    void $ Parsec.string "* Not offered"
            <|> Parsec.try revisedString
            <|> Parsec.lookAhead anySubjectHeader

revisedString :: UCSCParser String
revisedString = do
    Parsec.string "Revised"
    Parsec.optional $ Parsec.char ':'
    Parsec.char ' '
    Parsec.count 2 Parsec.digit
    Parsec.char '/'
    Parsec.count 2 Parsec.digit
    Parsec.char '/'
    (Parsec.try $ Parsec.count 4 Parsec.digit) <|> Parsec.count 2 Parsec.digit

subjectHeader :: Subject -> UCSCParser String
subjectHeader = Parsec.string . (map toUpper) . subjectName

courseHeader :: UCSCParser String
courseHeader = do
    (Parsec.char 'L' >> (Parsec.string "OWER-DIVISION COURSES"
                     <|> Parsec.string "ower-Division Courses"))
        <|> (Parsec.char 'U' >> ((Parsec.string "PPER"
                                  >> Parsec.oneOf " -"
                                  >> Parsec.string "DIVISION COURSES")
                             <|> Parsec.string "pper-Division Courses"))
        <|> (Parsec.char 'G' >> (Parsec.string "RADUATE COURSES"
                             <|> Parsec.string "raduate Courses"))

uniqueSubjectParser :: Subject -> (Bool, UCSCParser String)
uniqueSubjectParser HistoryArt = (True, Parsec.choice
    [ Parsec.string "Europe and the Americas"
    , Parsec.string "Modern Art and Visual Culture in \nEurope and the Americas"
    , Parsec.string "Renaissance"
    , Parsec.string "Oceania and its Diaspora"
    , Parsec.string "Cross-Regional Studies"
    ])
uniqueSubjectParser subject    = (False, return "")

getCourse :: Subject -> UCSCParser Course
getCourse sub = do
    courseBegin
    num     <- pack <$>                    getCourseNum
    name    <- pack <$> removeNewlines <$> getCourseName
    qrtrs   <-                             getCourseQuarters
    desc    <- pack <$> removeNewlines <$> getCourseDescription
    prereqs <- pack <$> removeNewlines <$> getCoursePrereqString
    gotoCourseEnd
    courseEnd

    let (unique, parser) = uniqueSubjectParser sub

    Parsec.optional $ Parsec.try $ newPage sub
    when unique $ Parsec.optional $ Parsec.try parser

    return Course
        { courseSubject = pack $ subjectName   sub
        , coursePrefix  = pack $ subjectPrefix sub
        , courseNumber  = num
        , courseName    = name
        , courseQrtrs   = pack $ show qrtrs
        , courseDesc    = desc
        , coursePreqs   = prereqs
        , coursePCmplt  = True
        }

courseBegin :: UCSCParser ()
courseBegin = do
    Parsec.spaces
    Parsec.optional $ Parsec.try courseHeader
    Parsec.spaces

courseEnd :: UCSCParser ()
courseEnd = do
    Parsec.space
    Parsec.spaces
    Parsec.try profName
        <|> (Parsec.string "The" >> Parsec.spaces >> Parsec.string "Staff")
    Parsec.optional $ Parsec.char ',' >> courseEnd
    Parsec.spaces

gotoCourseEnd :: UCSCParser String
gotoCourseEnd = Parsec.manyTill Parsec.anyChar $ Parsec.try $ Parsec.lookAhead courseEnd

newPage :: Subject -> UCSCParser ()
newPage subject = do
    Parsec.optional $ Parsec.many Parsec.digit >> newline'
    Parsec.string $ subjectSection subject
    spaces
    newline'

getCourseNum :: UCSCParser String
getCourseNum = do
    num <- Parsec.many1 Parsec.digit
    sub <- Parsec.many  Parsec.letter
    Parsec.char '.'
    Parsec.spaces
    return $ num ++ sub

getCourseName :: UCSCParser String
getCourseName = Parsec.manyTill Parsec.anyChar $ Parsec.try endName
    where endName = do
            Parsec.char '.'
            (Parsec.try $ do
                Parsec.many1 Parsec.space
                Parsec.lookAhead $ Parsec.oneOf "FSW*" >> void (Parsec.oneOf " ,*\n")
                ) <|> (spaces >> newline' >> Parsec.notFollowedBy Parsec.lower)

getCourseQuarters :: UCSCParser [Quarter]
getCourseQuarters = (Parsec.try $ do
    val    <- Parsec.oneOf "FSW*"
    others <- (Parsec.try $ do
        Parsec.spaces
        Parsec.char ','
        Parsec.spaces
        getCourseQuarters
        ) <|> (spaces >> Parsec.lookAhead newline' >> return [])

    return $ case val of
        'F' -> Fall   : others
        'S' -> Spring : others
        'W' -> Winter : others
        _   ->          others
    ) <|> (Parsec.spaces >> return [])

getCourseDescription :: UCSCParser String
getCourseDescription = Parsec.manyTill Parsec.anyChar
                                     $ Parsec.try $ Parsec.lookAhead descEnd
    where descEnd = Parsec.try (void $ Parsec.string "Prerequisite(s): ") <|> prereqEnd

getCoursePrereqString :: UCSCParser String
getCoursePrereqString = (Parsec.try $ do
    Parsec.string "Prerequisite(s):"
    Parsec.spaces
    Parsec.manyTill Parsec.anyChar $ Parsec.try $ Parsec.lookAhead prereqEnd
    ) <|> return ""

prereqEnd :: UCSCParser ()
prereqEnd = Parsec.try (void $ Parsec.string "(General") <|> courseEnd

profName :: UCSCParser String
profName = do
    Parsec.notFollowedBy $ Parsec.choice $ map Parsec.string $
        [ "I. Readings"
        , "A. The"
        ]
    initial <- Parsec.upper
    Parsec.char '.'
    Parsec.many1 Parsec.space
    first  <- Parsec.upper
    second <- Parsec.lower
    tail   <- Parsec.manyTill Parsec.letter $ Parsec.lookAhead $ Parsec.oneOf " -,\n"
    extra  <- Parsec.option "" $ Parsec.try $ do
        Parsec.oneOf " -"
        Parsec.optional newline'
        Parsec.notFollowedBy $ Parsec.string "Revised"
        first'  <- Parsec.upper
        second' <- Parsec.lower
        tail'   <- Parsec.manyTill Parsec.letter $ Parsec.lookAhead $ Parsec.oneOf " ,\n"
        return (' ':first':second':tail')
    endName
    return $ (initial : ". ") ++ (first:second:tail) ++ extra
    where endName = (Parsec.char ',' >> void courseEnd) <|> (spaces >> newline')

newline' :: UCSCParser ()
newline' = void $ Parsec.newline

spaces :: UCSCParser ()
spaces = void $ Parsec.many $ Parsec.char ' '

removeNewlines :: String -> String
removeNewlines = filter ((/=) '\n') . removeNewlines'

removeNewlines' :: String -> String
removeNewlines' (x:y:z:xs)
    | noSpace   = x : ' ' : z : removeNewlines' xs
    | newline   = x       : z : removeNewlines' xs
    | otherwise = x           : removeNewlines' (y : z : xs)
    where newline = y == '\n'
          noSpace = and [newline, x /= ' ', z /= ' ']
removeNewlines' xs = xs
