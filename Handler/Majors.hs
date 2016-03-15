--define new module of type handler with name "Majors"
module Handler.Majors where

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

--import simulator...
import Import
import Yesod
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Text.Blaze					  
import Text.Julius (RawJS (..))
import Data.Aeson.Types (Result (..))
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Settings
import Settings.StaticFiles
import Control.Applicative
import Data.Text (Text)
--------------------------------------------------------------------------------
-- *******************************************************
-- *** Begin resource handlers to receive get requests ***
-- *******************************************************
--This is the resource file containing links to UCSC's Majors
getProgressR :: Handler Html
getProgressR = do
 defaultLayout $ do
  setTitle "Get to Know your Majors!"
  $(widgetFile "majors")