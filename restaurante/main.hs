{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}

import Yesod
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)
             
data Pagina = Pagina {connPool :: ConnectionPool}

instance Yesod Pagina 

share[mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Clientes json
        nome Text
        telefone Text
        deriving Show
    
    |]



mkYesod "Pagina" [parseRoutes|
/cliente/cadastrar                CadastroCliente POST
|]

instance YesodPersist Pagina where
    type YesodPersistBackend Pagina = SqlBackend
    runDB f = do
    master <- getYesod
    let pool = connPool master
    runSqlPool f pool


------------------------------------------------------
optionsCadastroCliente :: ClientesId -> Handler()
optionsCadastroCliente cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "POST, OPTIONS"
        
postCadastroCliente :: Handler ()
postCadastroCliente = do
    addHeader "Access-Control-Allow-Origin" "*"
    cliente <- requireJsonBody :: Handler Clientes
    runDB $ insert cliente
    sendResponse (object [pack "resp" .= pack "CREATED"])
        


connStr = "dbname=d646s1j3kc48hp host=ec2-54-243-203-143.compute-1.amazonaws.com user=rrwiwpzzopujxv password=SUtpmoQKuaw-kY4XxsRamfoNb1 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)