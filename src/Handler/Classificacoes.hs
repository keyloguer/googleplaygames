{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Handler.Classificacoes where
import           Data.Text               (Text)
import Import
import Handler.Funcs

optionsClassificacoesR :: Handler ()
optionsClassificacoesR = headers

getClassificacoesR :: Handler Value
getClassificacoesR = do
        addHeader "Access-Control-Allow-Origin" "*"
        classificacoes <- runDB $ selectList [] [Asc ClassificacoesClassificacao]
        returnJson $ classificacoes