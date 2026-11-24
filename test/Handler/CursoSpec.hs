{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.CursoSpec (spec) where

import TestImport
import Data.Aeson (Value, encode, object, (.=))

-- | Envia uma requisição com corpo JSON.
enviarJson :: Text -> Route App -> Value -> YesodExample App ()
enviarJson metodo rota corpo = request $ do
    setMethod (encodeUtf8 metodo)
    setUrl rota
    addRequestHeader ("Content-Type", "application/json")
    addRequestHeader ("Accept", "application/json")
    setRequestBody (encode corpo)

cursoValido :: Value
cursoValido = object
    [ "nome" .= ("Ciência da Computação" :: Text)
    , "descricao" .= ("Bacharelado" :: Text)
    , "cargaHoraria" .= (3200 :: Int)
    ]

spec :: Spec
spec = withApp $ do
    describe "CRUD de Curso" $ do
        it "POST /cursos cria um curso (201)" $ do
            enviarJson "POST" CursosR cursoValido
            statusIs 201
            cursos <- runDB $ selectList ([] :: [Filter Curso]) []
            assertEq "um curso no banco" 1 (length cursos)

        it "POST /cursos com nome vazio retorna 400" $ do
            enviarJson "POST" CursosR $ object
                [ "nome" .= ("" :: Text), "cargaHoraria" .= (3200 :: Int) ]
            statusIs 400

        it "POST /cursos com nome repetido retorna 409" $ do
            enviarJson "POST" CursosR cursoValido
            enviarJson "POST" CursosR cursoValido
            statusIs 409

        it "GET, PUT e DELETE em /cursos/#CursoId" $ do
            cursoId <- runDB $ insert $ Curso "Direito" Nothing 3700

            get (CursoR cursoId)
            statusIs 200
            bodyContains "Direito"

            enviarJson "PUT" (CursoR cursoId) $ object
                [ "nome" .= ("Direito Noturno" :: Text), "cargaHoraria" .= (4000 :: Int) ]
            statusIs 200
            bodyContains "Direito Noturno"

            request $ setMethod "DELETE" >> setUrl (CursoR cursoId)
            statusIs 204
            restante <- runDB $ count [CursoId ==. cursoId]
            assertEq "curso removido" 0 restante

        it "GET /cursos/#CursoId inexistente retorna 404" $ do
            cursoId <- runDB $ insert $ Curso "Temporário" Nothing 100
            runDB $ delete cursoId
            get (CursoR cursoId)
            statusIs 404