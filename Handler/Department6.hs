module Handler.Department6 where

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

import Data.Text (unpack)
import qualified Data.Map as M

import Scraper.UCSC
import Scraper.Subjects

getLalsR :: Handler Html
getLalsR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LALS"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLtinR :: Handler Html
getLtinR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LTIN"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLgstR :: Handler Html
getLgstR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LGST"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLlingR :: Handler Html
getLlingR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LLING"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLitR :: Handler Html
getLitR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LIT"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getMathR :: Handler Html
getMathR = do
    courses <- runDB $ selectList [CoursePrefix ==. "MATH"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getMerrR :: Handler Html
getMerrR = do
    courses <- runDB $ selectList [CoursePrefix ==. "MERR"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getMetxR :: Handler Html
getMetxR = do
    courses <- runDB $ selectList [CoursePrefix ==. "METX"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLtmoR :: Handler Html
getLtmoR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LTMO"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getMuscR :: Handler Html
getMuscR = do
    courses <- runDB $ selectList [CoursePrefix ==. "MUSC"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getOaksR :: Handler Html
getOaksR = do
    courses <- runDB $ selectList [CoursePrefix ==. "OAKS"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getSpanR :: Handler Html
getSpanR = do
    courses <- runDB $ selectList [CoursePrefix ==. "SPAN"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getSphsR :: Handler Html
getSphsR = do
    courses <- runDB $ selectList [CoursePrefix ==. "SPHS"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLtspR :: Handler Html
getLtspR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LTSP"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getStevR :: Handler Html
getStevR = do
    courses <- runDB $ selectList [CoursePrefix ==. "STEV"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getTimR :: Handler Html
getTimR = do
    courses <- runDB $ selectList [CoursePrefix ==. "TIM"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getTheaR :: Handler Html
getTheaR = do
    courses <- runDB $ selectList [CoursePrefix ==. "THEA"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getUcdcR :: Handler Html
getUcdcR = do
    courses <- runDB $ selectList [CoursePrefix ==. "UCDC"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLtwlR :: Handler Html
getLtwlR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LTWL"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getWritR :: Handler Html
getWritR = do
    courses <- runDB $ selectList [CoursePrefix ==. "WRIT"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getYiddR :: Handler Html
getYiddR = do
    courses <- runDB $ selectList [CoursePrefix ==. "YIDD"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")