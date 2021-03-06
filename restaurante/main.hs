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
        
    Bebida json
        marca  Text
        litros Text
        preco  Double
        deriving Show
        
    Marmita json
        tamanho Int
        deriving Show 
    
    Entregador json  
        nome Text
        deriving Show
        
    Pedido json
        cliente ClientesId Maybe
        prato   PratoId Maybe
        marmita MarmitaId Maybe
        bebida  BebidaId Maybe
        qtde    Int
        entregador EntregadorId Maybe    
        deriving Show
        
    Comentario json
        cliente ClientesId Maybe
        comentario Text
        deriving Show    
    |]

mkYesod "Pagina" [parseRoutes|
/cliente/cadastrar                CadastroCliente POST
/cliente/mostrarTodos             MostrarClientes OPTIONS GET
/cliente/alterar/#ClientesId      AlterarCliente OPTIONS PUT
/cliente/deletar/#ClientesId      DeletarCliente OPTIONS DELETE
/prato/cadastrar                  CadastroPratoR POST
/prato/mostrarTodos               MostrarPratos OPTIONS GET
/prato/alterar/#PratoId           AlterarPrato OPTIONS PUT
/prato/deletar/#PratoId           DeletarPrato OPTIONS DELETE
/bebida/cadastrar                 CadastroBebidaR POST
/bebida/mostrarTodos              MostrarBebidas OPTIONS GET
/bebida/alterar/#BebidaId         AlterarBebida OPTIONS PUT 
/bebida/deletar/#BebidaId         DeletarBebida OPTIONS DELETE
/marmita/cadastrar                CadastroMarmitaR POST
/marmita/mostrarTodos             MostrarMarmitas OPTIONS GET
/marmita/alterar/#MarmitaId       AlterarMarmita OPTIONS PUT
/marmita/deletar/#MarmitaId       DeletarMarmita OPTIONS DELETE
/entregador/cadastrar             CadastroEntregadorR POST
/entregador/mostrarTodos          MostrarEntregador OPTIONS GET
/entregador/alterar/#EntregadorId AlterarEntregador OPTIONS PUT
/entregador/deletar/#EntregadorId DeletarEntregador OPTIONS DELETE
/pedido/cadastrar                 CadastroPedidoR POST
/pedido/mostrarTodos              MostrarPedido OPTIONS GET
/pedido/alterar/#PedidoId         AlterarPedido OPTIONS PUT
/pedido/deletar/#PedidoId         DeletarPedido OPTIONS DELETE
/comentarios/cadastrar            CadastroComentarioR POST
/comentarios/mostrarTodos         MostrarComentarios OPTIONS GET
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

optionsMostrarPratos :: Handler ()
optionsMostrarPratos = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "GET, OPTIONS"
    
getMostrarPratos :: Handler ()
getMostrarPratos = do
    addHeader "Access-Control-Allow-Origin" "*"
    prato <- runDB $ selectList [] [Asc PratoNome]
    sendResponse (object["pratos: " .= fmap toJSON prato])
    
optionsAlterarPrato :: PratoId -> Handler ()
optionsAlterarPrato cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "PUT, OPTIONS"
    
putAlterarPrato :: PratoId -> Handler ()
putAlterarPrato cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    prato <- requireJsonBody :: Handler Prato
    runDB $ update cid [PratoNome =. (pratoNome prato) ,
                        PratoValor =. (pratoValor prato)]
    sendResponse (object [pack "resp" .= pack "Changed"])  
    
optionsDeletarPrato :: PratoId -> Handler ()
optionsDeletarPrato cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "DELETE, OPTIONS"
    
deleteDeletarPrato :: PratoId -> Handler ()
deleteDeletarPrato cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    runDB $ delete cid
    sendResponse (object [pack "resp".= pack "Deleted"]) 
    
    -----------------------------------------------------------------    
     
optionsCadastroBebidaR :: BebidaId -> Handler ()
optionsCadastroBebidaR cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "POST, OPTIONS"
        
postCadastroBebidaR :: Handler ()
postCadastroBebidaR = do
    addHeader "Access-Control-Allow-Origin" "*"
    bebida <- requireJsonBody :: Handler Bebida
    runDB $ insert bebida
    sendResponse (object [pack "resp" .= pack "CREATED"])
    
optionsMostrarBebidas :: Handler ()
optionsMostrarBebidas = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "GET, OPTIONS"
    
getMostrarBebidas :: Handler ()
getMostrarBebidas = do
    addHeader "Access-Control-Allow-Origin" "*"
    bebida <- runDB $ selectList [] [Asc BebidaId]
    sendResponse (object["bebidas: " .= fmap toJSON bebida])

optionsAlterarBebida :: BebidaId -> Handler ()
optionsAlterarBebida cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "PUT, OPTIONS"
    
putAlterarBebida :: BebidaId -> Handler ()
putAlterarBebida cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    bebida <- requireJsonBody :: Handler Bebida
    runDB $ update cid [BebidaMarca =. (bebidaMarca bebida) ,
                        BebidaLitros =. (bebidaLitros bebida),
                        BebidaPreco =. (bebidaPreco bebida)]
    sendResponse (object [pack "resp" .= pack "Changed"]) 
    
optionsDeletarBebida :: BebidaId -> Handler ()
optionsDeletarBebida cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "DELETE, OPTIONS"

deleteDeletarBebida :: BebidaId -> Handler ()
deleteDeletarBebida cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    runDB $ delete cid
    sendResponse (object [pack "resp".= pack "Deleted"])
     
    ------------------------------------------------------------------      
    
optionsCadastroMarmitaR :: MarmitaId -> Handler ()
optionsCadastroMarmitaR cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "POST, OPTIONS"
    
postCadastroMarmitaR :: Handler ()
postCadastroMarmitaR = do
    addHeader "Access-Control-Allow-Origin" "*"
    marmita <- requireJsonBody :: Handler Marmita
    runDB $ insert marmita
    sendResponse (object [pack "resp" .= pack "CREATED"])   
    
optionsMostrarMarmitas :: Handler ()
optionsMostrarMarmitas = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "GET, OPTIONS"
    
getMostrarMarmitas :: Handler ()
getMostrarMarmitas = do
    addHeader "Access-Control-Allow-Origin" "*"
    marmita <- runDB $ selectList [] [Asc MarmitaId]
    sendResponse (object["marmitas: " .= fmap toJSON marmita]) 
    
optionsAlterarMarmita :: MarmitaId -> Handler ()
optionsAlterarMarmita cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "PUT, OPTIONS"

putAlterarMarmita :: MarmitaId -> Handler ()
putAlterarMarmita cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    marmita <- requireJsonBody :: Handler Marmita
    runDB $ update cid [MarmitaTamanho =. marmitaTamanho marmita ]
    sendResponse (object [pack "resp" .= pack "Changed"])    
    
optionsDeletarMarmita :: MarmitaId -> Handler ()
optionsDeletarMarmita cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "DELETE, OPTIONS"

deleteDeletarMarmita :: MarmitaId -> Handler ()
deleteDeletarMarmita cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    runDB $ delete cid
    sendResponse (object [pack "resp".= pack "Deleted"])
-------------------------------------------------------------------    

optionsCadastroEntregadorR :: EntregadorId -> Handler ()
optionsCadastroEntregadorR cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "POST, OPTIONS"
    
postCadastroEntregadorR :: Handler ()
postCadastroEntregadorR = do
    addHeader "Access-Control-Allow-Origin" "*"
    entregador <- requireJsonBody :: Handler Entregador
    runDB $ insert entregador
    sendResponse (object [pack "resp" .= pack "CREATED"])  
    
optionsMostrarEntregador :: Handler ()
optionsMostrarEntregador = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "GET, OPTIONS"
    
getMostrarEntregador :: Handler ()
getMostrarEntregador = do
    addHeader "Access-Control-Allow-Origin" "*"
    entregador <- runDB $ selectList [] [Asc EntregadorNome]
    sendResponse (object["entregadores: " .= fmap toJSON entregador]) 
    
optionsAlterarEntregador :: EntregadorId -> Handler ()
optionsAlterarEntregador cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "PUT, OPTIONS"

putAlterarEntregador :: EntregadorId -> Handler ()
putAlterarEntregador cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    entregador <- requireJsonBody :: Handler Entregador
    runDB $ update cid [EntregadorNome =. entregadorNome entregador ]
    sendResponse (object [pack "resp" .= pack "Changed"])
    
optionsDeletarEntregador :: EntregadorId -> Handler ()
optionsDeletarEntregador cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "DELETE, OPTIONS"

deleteDeletarEntregador :: EntregadorId -> Handler ()
deleteDeletarEntregador cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    runDB $ delete cid
    sendResponse (object [pack "resp".= pack "Deleted"])
--------------------------------------------------------------------

optionsCadastroPedidoR :: PedidoId -> Handler ()
optionsCadastroPedidoR cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "POST, OPTIONS"
    
postCadastroPedidoR :: Handler ()
postCadastroPedidoR = do
    addHeader "Access-Control-Allow-Origin" "*"
    pedido <- requireJsonBody :: Handler Pedido
    runDB $ insert pedido
    sendResponse (object [pack "resp" .= pack "CREATED"])
    
optionsMostrarPedido :: Handler ()
optionsMostrarPedido = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "GET, OPTIONS"
    
getMostrarPedido :: Handler ()
getMostrarPedido = do
    addHeader "Access-Control-Allow-Origin" "*"
    pedido <- runDB $ selectList [] [Asc PedidoId]
    sendResponse (object["pedido: " .= fmap toJSON pedido]) 
    
optionsAlterarPedido :: PedidoId -> Handler ()
optionsAlterarPedido cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "PUT, OPTIONS"

putAlterarPedido :: PedidoId -> Handler ()
putAlterarPedido cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    pedido <- requireJsonBody :: Handler Pedido
    runDB $ update cid [PedidoCliente =. (pedidoCliente pedido),
                        PedidoPrato =. (pedidoPrato pedido),
                        PedidoMarmita =. (pedidoMarmita pedido),
                        PedidoQtde =. (pedidoQtde pedido),
                        PedidoBebida =. (pedidoBebida pedido),
                        PedidoEntregador =. (pedidoEntregador pedido)]
    sendResponse (object [pack "resp" .= pack "Changed"]) 
    
optionsDeletarPedido :: PedidoId -> Handler ()
optionsDeletarPedido cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "DELETE, OPTIONS"

deleteDeletarPedido :: PedidoId -> Handler ()
deleteDeletarPedido cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    runDB $ delete cid
    sendResponse (object [pack "resp".= pack "Deleted"])        
-------------------------------------------------------------------- 

optionsCadastroComentarioR :: ComentarioId -> Handler ()
optionsCadastroComentarioR cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "POST, OPTIONS"
    
postCadastroComentarioR :: Handler ()
postCadastroComentarioR = do
    addHeader "Access-Control-Allow-Origin" "*"
    comentario <- requireJsonBody :: Handler Comentario
    runDB $ insert comentario
    sendResponse (object [pack "resp" .= pack "CREATED"])
    
optionsMostrarComentarios :: Handler ()
optionsMostrarComentarios = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "GET, OPTIONS"
    
getMostrarComentarios :: Handler ()
getMostrarComentarios = do
    addHeader "Access-Control-Allow-Origin" "*"
    comentario <- runDB $ selectList [] [Asc ComentarioId]
    sendResponse (object["comentarios: " .= fmap toJSON comentario])       
    
connStr = "dbname=d646s1j3kc48hp host=ec2-54-243-203-143.compute-1.amazonaws.com user=rrwiwpzzopujxv password=SUtpmoQKuaw-kY4XxsRamfoNb1 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)