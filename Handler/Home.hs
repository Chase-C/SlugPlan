module Handler.Home where

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Text.Julius (RawJS (..))

import Database.Persist
import Database.Persist.Sqlite

import Data.Text (unpack)
import qualified Data.Map as M

import Scraper.UCSC
import Scraper.Subjects

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    (pdfWidget,  pdfEnctype)  <- generateFormPost pdfForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        pdfSub     = Nothing :: Maybe String
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        [whamlet|
            <button .btn .btn-primary href=@{AllCoursesR}>Browse Courses
            <a href=@{AllCoursesR}>Browse courses|]
        $(widgetFile "homepage")


postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    (pdfWidget,  pdfEnctype)            <- generateFormPost pdfForm
    let handlerName = "postHomeR" :: Text
        submission  = case result of
            FormSuccess res -> Just res
            _               -> Nothing
        pdfSub      = Nothing :: Maybe String

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postPdfR :: Handler Html
postPdfR = do
    (formWidget, formEnctype)         <- generateFormPost sampleForm
    ((result, pdfWidget), pdfEnctype) <- runFormPost pdfForm
    let handlerName = "postPdfR" :: Text
    let submission  = Nothing :: Maybe (FileInfo, Text)
    pdfSub <- case result of
        FormSuccess res -> do
            --text <- liftIO $ parsePdf $ Data.Text.unpack $ fileName res
            --let subMap = getSubjectMap' text $ Data.Text.unpack $ fileName res
            subMap <- liftIO $ getSubjectMap $ Data.Text.unpack $ fileName res
            keys <- insertSubjectMap subMap
            return $ Just $ (show $ subMap) ++ " : " ++ (show $ take 4 keys)
            --case subMap of
            --    (Just m) -> do
            --        keys <- insertSubjectMap m
            --        return $ Just $ (show $ m) ++ " : " ++ (show $ take 4 keys)
            --        --return $ Just $ show keys
            --    _        -> return Nothing
        _                -> return Nothing

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

getAllCoursesR :: Handler Html
getAllCoursesR = do
    courses <- runDB $ selectList [] [Asc CourseSubject, Asc CourseNumber]
    defaultLayout
        [whamlet|
            <table style="width:100%">
                <tr>
                    <th>Subject
                    <th>Course Number
                    <th>Course Name
                    <th>Description
                $forall Entity courseid course <- courses
                    <tr>
                        <td>#{courseSubject course}
                        <td>#{courseNumber course}
                        <td><a href=@{CourseR courseid}>#{courseName course}
        |]
--            <ul>
 --               $forall Entity courseid course <- courses
   --                 <li>
     --                   <a href=@{CourseR courseid}>#{courseSubject course} #{courseNumber course}
getCourseR :: CourseId -> Handler String
getCourseR courseId = do
    course <- runDB $ get404 courseId
    return $ show course

insertSubjectMap :: SubjectMap -> Handler [Key Course]
--insertSubjectMap :: String -> Handler (Key Course)
--insertSubjectMap subMap = runDB $ insert $ Course "Pwning" "101" "How to Pwn"
insertSubjectMap subMap = --runDB $ concat <$> mapM (\(sub, courses) ->
    let subCourses = M.toList subMap
    in  runDB $ concat <$> mapM (\(sub, courses) ->
            let subName = pack $ subjectName sub
            in  mapM (\(num, name, preq) ->
                insert $ Course subName (pack num) (pack name) $ pack $ show preq
                ) courses
            ) subCourses
            --) subMap

sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing

pdfForm :: Form FileInfo
pdfForm = renderBootstrap3 BootstrapBasicForm $ fileAFormReq "Choose a PDF to parse"

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

