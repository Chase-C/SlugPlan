module Scraper.UCSC where
    --( parsePDF
    --, parseText
    --) where

import Prelude
import System.IO
import Pdf.Toolbox.Document
import Data.Maybe    (catMaybes)
import Data.Char     (toUpper)
import Data.List     (sort, group)
import Control.Monad (void)
import Data.Text     (unpack)

import Text.Parsec         ((<|>))
import Control.Applicative ((<*), (<$>))

import qualified Text.Parsec     as Parsec
import qualified Data.Map.Strict as Map

import Scraper.Subjects

type SubjectMap   = Map.Map Subject [(String, String, Maybe String)]
type UCSCParser a = Parsec.Parsec String () a

getSubjectMap :: FilePath -> IO SubjectMap
getSubjectMap filename = do
    input <- readFile filename
    let classes = getSubjectMap' input filename
    case classes of
        (Just cs) -> return cs
        _         -> return Map.empty

getSubjectMap' :: String -> String -> Maybe SubjectMap
getSubjectMap' input filename =
    let classes = Parsec.parse getAllSubjectCourses filename input
    in case classes of
        (Right cs) -> Just cs
        _          -> Nothing

getAllSubjectCourses :: UCSCParser SubjectMap
getAllSubjectCourses = Map.fromList <$> (Parsec.many $ Parsec.try getNextSubjectCourses)

getNextSubjectCourses :: UCSCParser (Subject, [(String, String, Maybe String)])
getNextSubjectCourses = do
    subject <- gotoNextSubject
    ( Parsec.try $ do
        courses <- getSubjectCourses subject
        return (subject, courses))
        <|> (Parsec.anyChar >> getNextSubjectCourses)

gotoNextSubject :: UCSCParser Subject
gotoNextSubject = do
    subStrn <- getSubStrn
    return $ fst $ head $ filter (\(_, s) -> s == subStrn) subjects
    where subjects   = map (\s -> (s, map toUpper $ subjectName s)) [AcadEnglish ..]
          getSubStrn = Parsec.try $
            (Parsec.try $ Parsec.lookAhead anySubjectHeader)
            <|> (Parsec.anyChar >> getSubStrn)

getSubjectCourses :: Subject -> UCSCParser [(String, String, Maybe String)]
getSubjectCourses sub = do
    subjectCoursesOpen sub
    Parsec.many getNextClass

anySubjectHeader :: UCSCParser String
anySubjectHeader = Parsec.try $ createMultiParser $
    map ((map toUpper) . subjectName) [AcadEnglish ..]

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
          subPar c = do
            Parsec.char c
            out <- createMultiParser $ map tail $ filter ((==) c . head) filtered
            return $ c : out

getNextClass :: UCSCParser (String, String, Maybe String)
getNextClass = Parsec.try $ Parsec.try classInfo
    <|> (Parsec.notFollowedBy (Parsec.string "Revised: " <|> anySubjectHeader)
            >> Parsec.anyChar
            >> getNextClass)

subjectCoursesOpen :: Subject -> UCSCParser String
subjectCoursesOpen sub =
    subjectHeader sub <* (Parsec.try $ Parsec.lookAhead $ parseWithinN 15 courseHeader)

subjectHeader :: Subject -> UCSCParser String
subjectHeader = Parsec.string . (map toUpper) . subjectName

parseWithinN :: Int -> UCSCParser a -> UCSCParser a
parseWithinN n p
    | n < 0     = fail "parseWithinN failed"
    | otherwise = Parsec.try p <|> (Parsec.anyChar >> parseWithinN (n - 1) p)

courseHeader :: UCSCParser String
courseHeader = do
    (Parsec.string "LOWER-DIVISION")
        <|> (Parsec.string "UPPER-DIVISION")
        <|> (Parsec.string "GRADUATE")
    Parsec.string " COURSES"

classInfo :: UCSCParser (String, String, Maybe String)
classInfo = do
    classBreak
    num   <- classNumber
    name  <- className
    preqs <- classPrereqs
    return (num, removeNewlines name, removeNewlines <$> preqs)

classNumber :: UCSCParser String
classNumber = do
    num <- Parsec.many1 Parsec.digit
    sub <- Parsec.many  Parsec.letter
    Parsec.char '.'
    return $ num ++ sub

className :: UCSCParser String
className =
    Parsec.many1 (Parsec.alphaNum <|> Parsec.oneOf " ,():\n") <* periodBreak

classPrereqs :: UCSCParser (Maybe String)
classPrereqs = Parsec.try getPrereqs
    <|> (Parsec.try (Parsec.lookAhead classBreak) >> return Nothing)
    <|> (Parsec.anyChar >> classPrereqs)

getPrereqs :: UCSCParser (Maybe String)
getPrereqs = do
    Parsec.string "Prerequisite(s): "
    prereqs <- Parsec.manyTill Parsec.anyChar $ Parsec.try
                                              $ Parsec.lookAhead classBreak
    return $ Just prereqs

periodBreak :: UCSCParser ()
periodBreak = Parsec.char '.' >> spaces

classBreak :: UCSCParser ()
classBreak = do
    (Parsec.try newline' <|> Parsec.try (void courseHeader))
    spaces >> newline' >> spaces

newline' :: UCSCParser ()
newline' = void $ Parsec.newline

spaces :: UCSCParser ()
spaces = void $ Parsec.many $ Parsec.char ' '

removeNewlines :: String -> String
removeNewlines = filter ((/=) '\n')

parsePdf :: FilePath -> IO String
parsePdf filename =
    withBinaryFile filename ReadMode $ \handle -> do
        res <- runPdfWithHandle handle knownFilters $ do
            pdf      <- document
            catalog  <- documentCatalog pdf
            rootNode <- catalogPageNode catalog
            kids     <- pageNodeKids rootNode
            count    <- pageNodeNKids rootNode

            concat <$> mapM (getPageText rootNode) [3..(count - 1)]
        case res of
            (Left err)   -> print err >> return ""
            (Right strn) -> return strn

getPageText :: (MonadPdf m, MonadIO m) => PageNode -> Int -> PdfE m String
getPageText node n = do
    page <- pageNodePageByNum node n
    text <- pageExtractText page
    return $ unpack text
