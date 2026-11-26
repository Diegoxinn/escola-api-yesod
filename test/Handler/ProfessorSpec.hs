{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ProfessorSpec (spec) where

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

professorValido :: Value
professorValido = object
    [ "nome" .= ("Ana Souza" :: Text)
    , "email" .= ("ana@escola.com" :: Text)
    , "especialidade" .= ("Programação Funcional" :: Text)
    ]

spec :: Spec
spec = withApp $ do
    describe "CRUD de Professor" $ do
        it "POST /professores cria um professor (201)" $ do
            enviarJson "POST" ProfessoresR professorValido
            statusIs 201
            professores <- runDB $
                selectList ([] :: [Filter Professor]) []
            assertEq "um professor no banco" 1 (length professores)

        it "POST /professores com campos vazios retorna 400" $ do
            enviarJson "POST" ProfessoresR $ object
                [ "nome" .= ("" :: Text)
                , "email" .= ("" :: Text)
                , "especialidade" .= ("" :: Text)
                ]
            statusIs 400

        it "POST /professores rejeita emails malformados" $ do
            forM_ ["sem-arroba", "ana@", "ana@escola", "ana@@escola.com"] $
                \email -> do
                    enviarJson "POST" ProfessoresR $ object
                        [ "nome" .= ("Ana Souza" :: Text)
                        , "email" .= (email :: Text)
                        , "especialidade" .= ("Matemática" :: Text)
                        ]
                    statusIs 400

        it "POST /professores aceita email com subdomínio e alias" $ do
            enviarJson "POST" ProfessoresR $ object
                [ "nome" .= ("Ana Souza" :: Text)
                , "email" .= ("ana.souza+prof@dept.escola.com" :: Text)
                , "especialidade" .= ("Matemática" :: Text)
                ]
            statusIs 201

        it "POST /professores com email repetido retorna 409" $ do
            enviarJson "POST" ProfessoresR professorValido
            enviarJson "POST" ProfessoresR professorValido
            statusIs 409

        it "GET, PUT e DELETE em /professores/#ProfessorId" $ do
            professorId <- runDB $ insert $
                Professor "Carlos Lima" "carlos@escola.com" "Banco de Dados"

            get (ProfessorR professorId)
            statusIs 200
            bodyContains "Carlos Lima"

            enviarJson "PUT" (ProfessorR professorId) $ object
                [ "nome" .= ("Carlos Oliveira" :: Text)
                , "email" .= ("carlos.oliveira@escola.com" :: Text)
                , "especialidade" .= ("Engenharia de Software" :: Text)
                ]
            statusIs 200
            bodyContains "Carlos Oliveira"

            request $ setMethod "DELETE" >> setUrl (ProfessorR professorId)
            statusIs 204
            restante <- runDB $ count [ProfessorId ==. professorId]
            assertEq "professor removido" 0 restante

        it "GET /professores/#ProfessorId inexistente retorna 404" $ do
            professorId <- runDB $ insert $
                Professor "Temporário" "temp@escola.com" "Geral"
            runDB $ delete professorId
            get (ProfessorR professorId)
            statusIs 404
