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

type SubjectMap   = Map.Map Subject [(String, String)]
type UCSCParser a = Parsec.Parsec String () a

parseText :: FilePath -> IO ()
parseText filename = do
    input <- readFile filename
    let classes = Parsec.parse getAllSubjectCourses filename input
    case classes of
        (Right cs) -> printMap cs
        (Left err) -> print err

printMap :: SubjectMap -> IO ()
printMap =
    mapM_ (\(subject, classes) -> do
        putStrLn $ subjectName subject
        mapM_ (\(num, name) -> putStrLn $ num ++ ". " ++ name) classes
        ) . Map.toList

test :: UCSCParser a -> UCSCParser a
test p = Parsec.try $ (Parsec.try p) <|> (Parsec.anyChar >> test p)

getSubjectMap :: String -> String -> Maybe SubjectMap
getSubjectMap input filename =
    let classes = Parsec.parse getAllSubjectCourses filename input
    in case classes of
        (Right cs) -> Just cs
        _          -> Nothing

getAllSubjectCourses :: UCSCParser SubjectMap
getAllSubjectCourses = Map.fromList <$> (Parsec.many $ Parsec.try getNextSubjectCourses)

getNextSubjectCourses :: UCSCParser (Subject, [(String, String)])
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

getSubjectCourses :: Subject -> UCSCParser [(String, String)]
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

getNextClass :: UCSCParser (String, String)
getNextClass = Parsec.try $ Parsec.try classNumName
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
    Parsec.space
    Parsec.string "COURSES"

classNumName :: UCSCParser (String, String)
classNumName = do
    classBreak
    num  <- classNumber
    name <- className
    return (num, removeNewlines name)

classNumber :: UCSCParser String
classNumber = do
    num <- Parsec.many1 Parsec.digit
    sub <- Parsec.many  Parsec.letter
    Parsec.char '.'
    return $ num ++ sub

className :: UCSCParser String
className =
    Parsec.many1 (Parsec.alphaNum <|> Parsec.oneOf " ,()\\") <* periodBreak

periodBreak :: UCSCParser ()
periodBreak = Parsec.char '.' >> Parsec.spaces

classBreak :: UCSCParser ()
classBreak = do
    (Parsec.try newline' <|> Parsec.try (void courseHeader))
    Parsec.spaces >> newline' >> Parsec.spaces

newline' :: UCSCParser ()
newline' = Parsec.string "\\n" >> return ()

removeNewlines :: String -> String
removeNewlines []     = []
removeNewlines (x:[]) = x:[]
removeNewlines (x:y:xs)
    | (x:y:[]) == "\\n" = removeNewlines xs
    | otherwise         = x : removeNewlines (y:xs)

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
            --mapM_ printKid kids
            --page <- pageNodePageByNum rootNode 2
            --printPage page
        case res of
            (Left err)   -> print err >> return ""
            (Right strn) -> return strn

printKid :: (MonadPdf m, MonadIO m) => Ref -> PdfE m ()
printKid kid = do
    pageNode <- loadPageNode kid
    case pageNode of
        (PageTreeNode node) -> do
            kids <- pageNodeKids node
            count <- pageNodeNKids node
            liftIO $ print $ "Node with " ++ show count ++ " children"
            mapM_ printKid kids
        (PageTreeLeaf leaf) -> do
            text <- pageExtractText leaf
            liftIO $ print text

getPageText :: (MonadPdf m, MonadIO m) => PageNode -> Int -> PdfE m String
getPageText node n = do
    page <- pageNodePageByNum node n
    text <- pageExtractText page
    return $ unpack text
