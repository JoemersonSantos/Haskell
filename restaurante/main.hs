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
    
    Prato json
        nome Text
        valor Double
        deriving Show    
    |]



mkYesod "Pagina" [parseRoutes|
/cliente/cadastrar                CadastroCliente POST
/cliente/mostrarTodos             MostrarClientes OPTIONS GET
/cliente/alterar/#ClientesId      AlterarCliente OPTIONS PUT
/cliente/deletar/#ClientesId      DeletarCliente OPTIONS DELETE
/prato/cadastrar                  CadastroPratoR POST
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
        
optionsMostrarClientes :: Handler ()
optionsMostrarClientes = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "GET, OPTIONS"
        
getMostrarClientes :: Handler ()
getMostrarClientes = do
    addHeader "Access-Control-Allow-Origin" "*"
    clientes <- runDB $ selectList [] [Asc ClientesNome]
    sendResponse (object["clientes" .= fmap toJSON clientes])

optionsAlterarCliente :: ClientesId -> Handler ()
optionsAlterarCliente cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "PUT, OPTIONS"
    
putAlterarCliente :: ClientesId -> Handler ()
putAlterarCliente cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    cliente <- requireJsonBody :: Handler Clientes
    runDB $ update cid [ClientesNome =. (clientesNome cliente),
                        ClientesTelefone =. (clientesTelefone cliente)]
    sendResponse (object [pack "resp" .= pack "Changed"])
    
optionsDeletarCliente :: ClientesId -> Handler ()
optionsDeletarCliente cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "DELETE, OPTIONS"
    
deleteDeletarCliente :: ClientesId -> Handler ()
deleteDeletarCliente cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    runDB $ delete cid
    sendResponse (object [pack "resp".= pack "Deleted"])       
    
    ------------------------------------------------------------    
optionsCadastroPratoR :: PratoId -> Handler ()
optionsCadastroPratoR cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "POST, OPTIONS"

postCadastroPratoR :: Handler ()
postCadastroPratoR = do
    addHeader "Access-Control-Allow-Origin" "*"
    prato <- requireJsonBody :: Handler Prato
    runDB $ insert prato
    sendResponse (object [pack "resp" .= pack "CREATED"])    
    
connStr = "dbname=d646s1j3kc48hp host=ec2-54-243-203-143.compute-1.amazonaws.com user=rrwiwpzzopujxv password=SUtpmoQKuaw-kY4XxsRamfoNb1 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)