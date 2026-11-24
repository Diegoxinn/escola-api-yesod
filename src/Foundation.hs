{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Foundation where

import Import.NoFoundation
import Data.Kind            (Type)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamlet)
import Control.Monad.Logger (LogSource)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

-- | Tipo principal da aplicação. Guarda as configurações e o pool de
-- conexões com o SQLite, acessíveis em todos os handlers.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static          -- ^ Arquivos estáticos (favicon etc.)
    , appConnPool    :: ConnectionPool  -- ^ Pool de conexões com o SQLite
    , appHttpManager :: Manager         -- ^ Cliente HTTP (usado na API externa)
    , appLogger      :: Logger
    }

-- As rotas são definidas em config/routes.yesodroutes
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | Atalho para funções que acessam o banco.
type DB a = forall (m :: Type -> Type).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

-- | API REST: sem sessão, sem login e sem páginas HTML.
instance Yesod App where
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- API REST é stateless: não usa cookies de sessão.
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = return Nothing

    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    -- Sem autenticação: todas as rotas são liberadas.
    isAuthorized :: Route App -> Bool -> Handler AuthResult
    isAuthorized _ _ = return Authorized

    -- Layout mínimo, usado só pelas páginas de erro em HTML.
    -- Requisições com "Accept: application/json" recebem erros em JSON.
    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        withUrlRenderer [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle pc}
                    ^{pageHead pc}
                <body>
                    ^{pageBody pc}
            |]

    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- | Como executar ações no banco (SQLite) a partir dos handlers.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger