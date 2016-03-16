module Handler.LogIn where


{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Import
import Yesod
import Control.Applicative
import Data.Text (Text)
import Database.Persist

data Input = Input

-- Google client ID. 
clientId :: Text
clientId = "488381746948-hqnuulhpu1kotloi562d9pf7stkpk18h.apps.googleusercontent.com"

-- Google secret ID.
clientSecret :: Text
clientSecret = "eiOFqIa6Y4epKymLX8AccRFl"

--Routes added to 'routes'
{-}
mkYesod "Input" [parseRoutes|
/ SignUpR GET
/input InputR GET
|]
-}

--instance Yesod Input

instance RenderMessage Input FormMessage where
    renderMessage _ _ = defaultFormMessage

data Person = Person { personName :: Text, personPassword :: Text }
    deriving Show

getLogInR :: Handler Html
getLogInR = do 
    defaultLayout $ do
        setTitle "Log In"
        $(widgetFile "login")

getLogInputR :: Handler Html
getLogInputR = do
    person <- runInputGet $ Person
                <$> ireq textField "name"
                <*> ireq textField "password"
    defaultLayout [whamlet|<p>#{show person}|]



