{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.DisciplinaSpec (spec) where

import TestImport
import Data.Aeson (Value, encode, object, toJSON, (.=))

-- | Envia uma requisição com corpo JSON.
enviarJson :: Text -> Route App -> Value -> YesodExample App ()
enviarJson metodo rota corpo = request $ do
    setMethod (encodeUtf8 metodo)
    setUrl rota
    addRequestHeader ("Content-Type", "application/json")
    addRequestHeader ("Accept", "application/json")
    setRequestBody (encode corpo)

disciplinaJson :: CursoId -> Text -> Int -> Value
disciplinaJson cursoId nome cargaHoraria =
    toJSON $ Disciplina nome cargaHoraria cursoId

spec :: Spec
spec = withApp $ do
    describe "CRUD de Disciplina" $ do
        it "POST /disciplinas cria uma disciplina (201)" $ do
            cursoId <- runDB $ insert $
                Curso "Ciência da Computação" Nothing 3200
            enviarJson "POST" DisciplinasR $
                disciplinaJson cursoId "Haskell" 60
            statusIs 201
            disciplinas <- runDB $
                selectList ([] :: [Filter Disciplina]) []
            assertEq "uma disciplina no banco" 1 (length disciplinas)

        it "POST /disciplinas com dados inválidos retorna 400" $ do
            cursoId <- runDB $ insert $
                Curso "Ciência da Computação" Nothing 3200
            enviarJson "POST" DisciplinasR $
                disciplinaJson cursoId "  " 0
            statusIs 400

        it "POST /disciplinas com curso inexistente retorna 400" $ do
            cursoId <- runDB $ insert $
                Curso "Temporário" Nothing 100
            runDB $ delete cursoId
            enviarJson "POST" DisciplinasR $
                disciplinaJson cursoId "Haskell" 60
            statusIs 400

        it "GET, PUT e DELETE em /disciplinas/#DisciplinaId" $ do
            (cursoId, disciplinaId) <- runDB $ do
                cursoId <- insert $ Curso "Direito" Nothing 3700
                disciplinaId <- insert $
                    Disciplina "Direito Civil" 60 cursoId
                return (cursoId, disciplinaId)

            get (DisciplinaR disciplinaId)
            statusIs 200
            bodyContains "Direito Civil"

            enviarJson "PUT" (DisciplinaR disciplinaId) $
                disciplinaJson cursoId "Direito Penal" 80
            statusIs 200
            bodyContains "Direito Penal"

            request $ setMethod "DELETE" >> setUrl (DisciplinaR disciplinaId)
            statusIs 204
            restante <- runDB $ count [DisciplinaId ==. disciplinaId]
            assertEq "disciplina removida" 0 restante

        it "DELETE /disciplinas com turmas vinculadas retorna 409" $ do
            disciplinaId <- runDB $ do
                cursoId <- insert $ Curso "Engenharia" Nothing 3000
                professorId <- insert $
                    Professor "Ana Souza" "ana@escola.com" "Matemática"
                disciplinaId <- insert $
                    Disciplina "Cálculo" 80 cursoId
                _ <- insert $
                    Turma "CALC-2026-1" "2026.1" disciplinaId professorId
                return disciplinaId

            request $ setMethod "DELETE" >> setUrl (DisciplinaR disciplinaId)
            statusIs 409

        it "GET /disciplinas/#DisciplinaId inexistente retorna 404" $ do
            cursoId <- runDB $ insert $
                Curso "Temporário" Nothing 100
            disciplinaId <- runDB $ insert $
                Disciplina "Temporária" 30 cursoId
            runDB $ delete disciplinaId
            get (DisciplinaR disciplinaId)
            statusIs 404
