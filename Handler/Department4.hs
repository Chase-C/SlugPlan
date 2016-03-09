module Handler.Department4 where

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

getCleiR :: Handler Html
getCleiR = do
    courses <- runDB $ selectList [CoursePrefix ==. "CLEI"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getClniR :: Handler Html
getClniR = do
    courses <- runDB $ selectList [CoursePrefix ==. "CLNI"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getClteR :: Handler Html
getClteR = do
    courses <- runDB $ selectList [CoursePrefix ==. "CLTE"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getCmmuR :: Handler Html
getCmmuR = do
    courses <- runDB $ selectList [CoursePrefix ==. "CMMU"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getCmpmR :: Handler Html
getCmpmR = do
    courses <- runDB $ selectList [CoursePrefix ==. "CMPM"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getCmpeR :: Handler Html
getCmpeR = do
    courses <- runDB $ selectList [CoursePrefix ==. "CMPE"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getCmpsR :: Handler Html
getCmpsR = do
    courses <- runDB $ selectList [CoursePrefix ==. "CMPS"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getCowlR :: Handler Html
getCowlR = do
    courses <- runDB $ selectList [CoursePrefix ==. "COWL"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getLtcrR :: Handler Html
getLtcrR = do
    courses <- runDB $ selectList [CoursePrefix ==. "LTCR"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getCresR :: Handler Html
getCresR = do
    courses <- runDB $ selectList [CoursePrefix ==. "Cres"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getCrwnR :: Handler Html
getCrwnR = do
    courses <- runDB $ selectList [CoursePrefix ==. "CRWN"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getDanmR :: Handler Html
getDanmR = do
    courses <- runDB $ selectList [CoursePrefix ==. "DANM"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getEartR :: Handler Html
getEartR = do
    courses <- runDB $ selectList [CoursePrefix ==. "EART"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")
getBioeR :: Handler Html
getBioeR = do
    courses <- runDB $ selectList [CoursePrefix ==. "BIOE"] []
    defaultLayout $ do
        setTitle "SlugPlan: Browse Courses"
        $(widgetFile "browsecourses")