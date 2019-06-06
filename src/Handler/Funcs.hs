{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Funcs where

import Import
import Data.Text as T


(%=.) :: EntityField record Text -> Text -> Filter record
(%=.) campo valor = Filter campo ( Left $ T.concat ["%",valor,"%"] )
                                 ( BackendSpecificFilter "ILIKE" )

headers :: Handler ()
headers = do 
    addHeader "Access-Control-Allow-Methods" "*"
    addHeader "Access-Control-Allow-Headers" "*"
    addHeader "Access-Control-Allow-Origin" "*"