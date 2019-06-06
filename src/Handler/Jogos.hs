{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Handler.Jogos where
import           Data.Text               (Text)
import Handler.Funcs
import Import
import GHC.Generics
import Database.Persist.Sql (toSqlKey, fromSqlKey)

data JogosData = JogosData Jogos Desenvolvedores Classificacoes Generos deriving (Show ,Generic, ToJSON, FromJSON)

optionsJogosR :: Handler ()
optionsJogosR = headers

getJogosR :: Handler Value
getJogosR = do
        addHeader "Access-Control-Allow-Origin" "*"
        generos <- runDB $ selectList [] [Asc GenerosGenero]
        generosId <- return $ fmap entityKey generos
        jogosCategoria <- mapM (\genId -> runDB $ selectList [JogosGeneroId ==. genId] [Asc JogosTitulo, LimitTo 16]) generosId
        foo <- sequence $ map (\ jogos -> sequence $ map (\ jogo@(Entity jid (Jogos did _ _ _ _ _ _ _ genid clid _ _ _ _ _)) -> do 
                (dev, cla, gen) <- select did clid genid
                return $ JogosData (entityVal jogo) dev cla gen
                ) jogos )  jogosCategoria
        sendStatusJSON ok200 (toJSON $ foo)
        where select :: DesenvolvedoresId -> ClassificacoesId -> GenerosId -> Handler (Desenvolvedores,Classificacoes,Generos)
              select did claid genid = runDB $ do 
                                            dev <- get404 did 
                                            cla <- get404 claid
                                            gen  <- get404 genid
                                            return $ (dev,cla,gen)
                                            
optionsJogosDevR :: Handler ()
optionsJogosDevR = headers

getJogosDevR :: Handler Value
getJogosDevR = do
        addHeader "Access-Control-Allow-Origin" "*"
        jogos <- runDB $ selectList [] [Asc JogosTitulo, LimitTo 500]
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
                                            
postJogosDevR :: Handler Value
postJogosDevR = do
    addHeader "Access-Control-Allow-Origin" "*"
    jogo <- requireJsonBody :: Handler Jogos
    ultKey <- runDB $ do
        Just ultJogo <- selectFirst [] [Desc JogosId]
        let ultKey = toSqlKey . (+1) . fromSqlKey . entityKey $ ultJogo
        insertKey ultKey jogo
        return ultKey
    sendStatusJSON created201 (object ["jogoId" .= ultKey])
    
    
optionsJogosIdR ::  JogosId -> Handler ()
optionsJogosIdR _ = headers
    
putJogosIdR :: JogosId -> Handler TypedContent 
putJogosIdR jogoId = do                        
    addHeader "Access-Control-Allow-Origin" "*"
    jogo <- requireJsonBody :: Handler Jogos
    runDB $ replace jogoId jogo
    sendStatusJSON noContent204 (Aviso "editado")