module Handler.Planner where

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import Import hiding (head, last)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Settings
import Settings.StaticFiles
import Text.Julius (RawJS (..))
import Data.Aeson.Types (Result (..))

import Database.Persist
import Database.Persist.Sql

import Control.Applicative ((<$>))
import Data.Bits           ((.&.))
import Data.List           ((!!), head, last)

--------------------------------------------------------------------------------

getPlannerR :: Handler Html
getPlannerR = do
    qtrGroups <- groupBy (\(n1, _, _) (n2, _, _) -> n1 == n2) <$> getQtrCourses
    let qtrCourses      = groupCourses qtrGroups
        (lastQtr, _, _) = last qtrCourses
    defaultLayout $ do
        setTitle "Course Planner"
        $(widgetFile "planner")

getQtrCourses :: Handler [(Int, Text, (Course, CourseId))]
getQtrCourses =
    runDB $ do
        cs <- selectList [UserCourseUser ==. 1] [Asc UserCourseQuarter]
        catMaybes <$>
          mapM (\usrCourse ->
                   let id  = userCourseCourse  $ entityVal usrCourse
                       qtr = userCourseQuarter $ entityVal usrCourse
                   in  do
                       course <- get id
                       case course of
                           (Just c) -> return $ Just (qtr, getQuarter qtr, (c, id))
                           Nothing  -> return Nothing) cs

groupCourses :: [[(Int, Text, (Course, CourseId))]]
             -> [(Int, Text, [[(Course, CourseId)]])]
groupCourses []     = []
groupCourses (x:xs) =
    let (num, qtr, _) = head x
        cs            = map (\(_, _, c) -> c) x
    in  (num, qtr, groupThree cs) : groupCourses xs

groupThree :: [a] -> [[a]]
groupThree (a:b:c:xs) = [a,b,c] : groupThree xs
groupThree []         = []
groupThree xs         = [xs]

getQuarter :: Int -> Text
getQuarter n =
    let year = 2016 + ((n + 1) `div` 4)
        qtr  = ["Winter", "Spring", "Summer", "Fall"] !! ((n + 1) .&. 3)
    in  qtr ++ " " ++ tshow year

deleteNewCourseR :: Handler Value
deleteNewCourseR = do
    courseId <- (requireJsonBody :: Handler CourseId)
    runDB $ deleteWhere [UserCourseCourse ==. courseId]
    returnJson ("ok" :: Text)

putNewCourseR :: Handler Value
putNewCourseR =
    returnJson ("ok" :: Text)

postNewCourseR :: Handler Value
postNewCourseR = do
    userCourse <- (requireJsonBody :: Handler UserCourse)
    mCourse    <- runDB $ do
        c <- get $ userCourseCourse userCourse
        insert $ UserCourse 1 (userCourseCourse  userCourse)
                              (userCourseQuarter userCourse)
        return c
    case mCourse of
        (Just course) -> returnJson course
        _             -> notFound

getNewCourseR :: Handler Value
getNewCourseR =
    returnJson ("ok" :: Text)

postNewQuarterR :: Handler Value
postNewQuarterR =
    returnJson ("ok" :: Text)

getSearchCourseR :: Text -> Handler Value
getSearchCourseR searchStrn = do
    courses <- runDB $ matchCourses searchStrn
    returnJson $ take 7 courses

courseIds :: (Text, Text)
courseIds = ("js-courseForm", "js-courseList")

matchCourses :: MonadIO m => Text -> ReaderT SqlBackend m [Entity Course]
matchCourses strn = rawSql sqlStrn values
    where fields     = ["subject", "prefix", "number", "name"]
          strns      = words strn
          numStrns   = length strns
          searchOrs  = intercalate " or " $ map (\f -> f ++ " like ?") fields
          searchAnds = intercalate " and " $ replicate numStrns ("(" ++ searchOrs ++ ")")
          sqlStrn    = "select ?? from course where " ++ searchAnds
          values     = concat $ map (\s -> map PersistText $ replicate 4 $ surr s) strns
          surr s     = "%" ++ s ++ "%"
