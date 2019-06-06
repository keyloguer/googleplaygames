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
import Handler.Funcs
import GHC.Generics

data JogosData = JogosData Jogos Desenvolvedores Classificacoes Generos deriving (Show ,Generic, ToJSON, FromJSON)
data Jogo = Jogo { titulo :: Text, desenvolvedorId :: DesenvolvedoresId } deriving (Show ,Generic, ToJSON, FromJSON)
data Desenvolvedor = Desenvolvedor { ds_desenvolvedor :: Text, emailDesenvolvedor :: Text }  deriving (Show ,Generic, ToJSON, FromJSON)

optionsJogosDevGeneroR :: Handler ()
optionsJogosDevGeneroR = headers

getJogosDevGeneroR :: DesenvolvedoresId -> GenerosId -> Handler Value
getJogosDevGeneroR devId genId = do
        addHeader "Access-Control-Allow-Origin" "*"
        jogos <- runDB $ selectList [JogosDesenvolvedorid ==. devId, JogosGeneroId ==. genId] [Asc JogosTitulo]
        jogosComInnerJoin <- mapM (\ jogo@(Entity jid (Jogos did _ _ _ _ _ _ _ genid clid _ _ _ _ _)) -> do 
                             (dev, cla, gen) <- select did clid genid
                             return $ JogosData (entityVal jogo) dev cla gen
                             ) jogos
        sendStatusJSON ok200 (toJSON $ jogosComInnerJoin)
        where select :: DesenvolvedoresId -> ClassificacoesId -> GenerosId -> Handler (Desenvolvedores,Classificacoes,Generos)
              select did claid genid = runDB $ do 
                                            dev <- get404 did 
                                            cla <- get404 claid
                                            gen  <- get404 genid
                                            return $ (dev,cla,gen)

--Pesquisa jogo por Genero
getJogosGeneroR :: GenerosId -> Handler Value
getJogosGeneroR genId = do
        jogos <- runDB $ selectList [JogosGeneroId ==. genId] [Asc JogosTitulo]
        returnJson $ jogos