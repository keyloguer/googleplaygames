{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Handler.Desenvolvedores where
import           Data.Text               (Text)
import Import
import Handler.Funcs

getDesenvolvedoresR :: Handler Value
getDesenvolvedoresR = do
        addHeader "Access-Control-Allow-Origin" "*"
        desenvolvedores <- runDB $ selectList [] [Asc DesenvolvedoresEmailDesenvolvedor] --
        returnJson $ desenvolvedores