{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.TurmaSpec (spec) where

import TestImport
import Data.Aeson (Value, encode, toJSON)

-- | Envia uma requisição com corpo JSON.
enviarJson :: Text -> Route App -> Value -> YesodExample App ()
enviarJson metodo rota corpo = request $ do
    setMethod (encodeUtf8 metodo)
    setUrl rota
    addRequestHeader ("Content-Type", "application/json")
    addRequestHeader ("Accept", "application/json")
    setRequestBody (encode corpo)

turmaJson :: Text -> Text -> DisciplinaId -> ProfessorId -> Value
turmaJson codigo semestre disciplinaId professorId =
    toJSON $ Turma codigo semestre disciplinaId professorId

criarReferencias :: YesodExample App (DisciplinaId, ProfessorId)
criarReferencias = runDB $ do
    cursoId <- insert $ Curso "Ciência da Computação" Nothing 3200
    disciplinaId <- insert $ Disciplina "Haskell" 60 cursoId
    professorId <- insert $
        Professor "Ana Souza" "ana@escola.com" "Programação Funcional"
    return (disciplinaId, professorId)

spec :: Spec
spec = withApp $ do
    describe "CRUD de Turma" $ do
        it "POST /turmas cria uma turma (201)" $ do
            (disciplinaId, professorId) <- criarReferencias
            enviarJson "POST" TurmasR $
                turmaJson "HASK-2026-1" "2026.1" disciplinaId professorId
            statusIs 201
            turmas <- runDB $ selectList ([] :: [Filter Turma]) []
            assertEq "uma turma no banco" 1 (length turmas)

        it "GET /turmas lista as turmas" $ do
            (disciplinaId, professorId) <- criarReferencias
            _ <- runDB $ insert $
                Turma "HASK-2026-1" "2026.1" disciplinaId professorId
            get TurmasR
            statusIs 200
            bodyContains "HASK-2026-1"

        it "POST /turmas com campos obrigatórios vazios retorna 400" $ do
            (disciplinaId, professorId) <- criarReferencias
            enviarJson "POST" TurmasR $
                turmaJson "  " "" disciplinaId professorId
            statusIs 400

        it "POST /turmas com código repetido retorna 409" $ do
            (disciplinaId, professorId) <- criarReferencias
            let turma = turmaJson "HASK-2026-1" "2026.1"
                    disciplinaId professorId
            enviarJson "POST" TurmasR turma
            enviarJson "POST" TurmasR turma
            statusIs 409

        it "POST /turmas com referências inexistentes retorna 400" $ do
            (disciplinaId, professorId) <- criarReferencias
            runDB $ delete disciplinaId
            enviarJson "POST" TurmasR $
                turmaJson "HASK-2026-1" "2026.1" disciplinaId professorId
            statusIs 400

        it "GET, PUT e DELETE em /turmas/#TurmaId" $ do
            (disciplinaId, professorId) <- criarReferencias
            turmaId <- runDB $ insert $
                Turma "HASK-2026-1" "2026.1" disciplinaId professorId

            get (TurmaR turmaId)
            statusIs 200
            bodyContains "HASK-2026-1"

            enviarJson "PUT" (TurmaR turmaId) $
                turmaJson "HASK-2026-2" "2026.2" disciplinaId professorId
            statusIs 200
            bodyContains "HASK-2026-2"

            request $ setMethod "DELETE" >> setUrl (TurmaR turmaId)
            statusIs 204
            restante <- runDB $ count [TurmaId ==. turmaId]
            assertEq "turma removida" 0 restante

        it "DELETE /turmas com matrículas vinculadas retorna 409" $ do
            turmaId <- runDB $ do
                cursoId <- insert $ Curso "Engenharia" Nothing 3000
                disciplinaId <- insert $
                    Disciplina "Cálculo" 80 cursoId
                professorId <- insert $
                    Professor "Ana Souza" "ana@escola.com" "Matemática"
                alunoId <- insert $ Aluno "João Silva" "joao@escola.com"
                    "RA001" (fromGregorian 2004 5 10) cursoId
                turmaId <- insert $
                    Turma "CALC-2026-1" "2026.1" disciplinaId professorId
                _ <- insert $ Matricula alunoId turmaId (fromGregorian 2026 2 1)
                return turmaId

            request $ setMethod "DELETE" >> setUrl (TurmaR turmaId)
            statusIs 409

        it "GET /turmas/#TurmaId inexistente retorna 404" $ do
            (disciplinaId, professorId) <- criarReferencias
            turmaId <- runDB $ insert $
                Turma "TEMP-2026-1" "2026.1" disciplinaId professorId
            runDB $ delete turmaId
            get (TurmaR turmaId)
            statusIs 404
