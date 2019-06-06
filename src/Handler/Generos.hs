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
        --O nome do Arquivo não tem nada a ver com a query no banco...
data Jogo = Jogo { titulo :: Text, desenvolvedorId :: DesenvolvedoresId } deriving (Show ,Generic, ToJSON, FromJSON)

data Desenvolvedor = Desenvolvedor { ds_desenvolvedor :: Text, emailDesenvolvedor :: Text }  deriving (Show ,Generic, ToJSON, FromJSON)


data JogosData = JogosData Jogos Desenvolvedores Classificacoes Generos deriving (Show ,Generic, ToJSON, FromJSON)

        --NÃO FUNCIONA ESSA PORRA PQ!!!!
getTesteR :: Handler Value
getTesteR = do          --  Handler [Entity] . Monad BD Persst
        jogos <- runDB $ selectList [] [Asc JogosTitulo, LimitTo 3] -- [Entity Key Val]
        let jogosVal = fmap (\ (Entity _ val ) -> val ) jogos -- [Val]
        let jogosDidTitulo = fmap (\ ( Jogos did _ titulo _ _ _ _ _ _ _ _ _ _ _ _) -> Jogo titulo did ) jogosVal
        let desenvolvedorId = fmap (\(Jogo _ did) -> did) jogosDidTitulo
        desenvolvedores <- runDB $ selectList [DesenvolvedoresId <-. desenvolvedorId] []
        let desenvolvedoresVal = fmap (\ (Entity _ val ) -> val ) desenvolvedores
        let desenvolvedorDesc = fmap (\ ( Desenvolvedores descricao email _) -> Desenvolvedor descricao email) desenvolvedoresVal
        returnJson $ desenvolvedorDesc--dto
        
optionsGenerosR :: Handler ()
optionsGenerosR = headers

-- Retornar lista de generos para montar pesquisa por genero
getGenerosR :: Handler Value
getGenerosR = do
        addHeader "Access-Control-Allow-Origin" "*"
        generos <- runDB $ selectList [] [Asc GenerosGenero]
        returnJson $ generos

optionsJogosDevGeneroR :: Handler ()
optionsJogosDevGeneroR = headers

getJogosDevGeneroR :: DesenvolvedoresId -> GenerosId -> Handler Value
getJogosDevGeneroR devId genId = do
        addHeader "Access-Control-Allow-Origin" "*"
        jogos <- runDB $ selectList [JogosDesenvolvedorid ==. devId, JogosGeneroId ==. genId] [Asc JogosTitulo]
        jogosComInnerJoin <- mapM (\ jogo@(Entity _ (Jogos did _ _ _ _ _ _ _ genid clid _ _ _ _ _)) -> do 
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