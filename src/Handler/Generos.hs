{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Handler.Generos where
import           Data.Text               (Text)
import Import
import GHC.Generics

--Pesquisa jogo por Genero
getJogosGeneroR :: GenerosId -> Handler Value
getJogosGeneroR genId = do
        jogos <- runDB $ selectList [JogosGeneroId ==. genId] [Asc JogosTitulo]
        returnJson $ jogos