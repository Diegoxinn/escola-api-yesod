{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.MatriculaSpec (spec) where

import TestImport
import Data.Aeson (Value, encode, object, (.=))
import Data.Time.Calendar (fromGregorian)
import Database.Persist.Sql (fromSqlKey)

-- manda request com json
enviarJson :: Text -> Route App -> Value -> YesodExample App ()
enviarJson metodo rota corpo = request $ do
    setMethod (encodeUtf8 metodo)
    setUrl rota
    addRequestHeader ("Content-Type", "application/json")
    addRequestHeader ("Accept", "application/json")
    setRequestBody (encode corpo)

-- cria curso, professor, disciplina e turma (matricula precisa de tudo isso)
criarTurma :: YesodExample App (CursoId, TurmaId)
criarTurma = runDB $ do
    cursoId      <- insert $ Curso "Sistemas" Nothing 3200
    professorId  <- insert $ Professor "Carlos" "carlos@escola.com" "Haskell"
    disciplinaId <- insert $ Disciplina "Programação Funcional" 80 cursoId
    turmaId      <- insert $ Turma "T1" "2026.1" disciplinaId professorId
    return (cursoId, turmaId)

-- cria aluno com email e ra derivados do nome
criarAluno :: CursoId -> Text -> YesodExample App AlunoId
criarAluno cursoId nome = runDB $ insert $
    Aluno nome (nome <> "@escola.com") ("RA-" <> nome) (fromGregorian 2000 5 10) cursoId

-- json de matricula pra reaproveitar
matriculaJson :: AlunoId -> TurmaId -> Text -> Value
matriculaJson alunoId turmaId dia = object
    [ "alunoId"       .= alunoId
    , "turmaId"       .= turmaId
    , "dataMatricula" .= dia
    ]

spec :: Spec
spec = withApp $ do
    describe "CRUD de Matricula" $ do
        it "POST /matriculas cria uma matricula (201)" $ do
            (cursoId, turmaId) <- criarTurma
            alunoId <- criarAluno cursoId "Ana"
            enviarJson "POST" MatriculasR (matriculaJson alunoId turmaId "2026-02-01")
            statusIs 201
            matriculas <- runDB $ selectList ([] :: [Filter Matricula]) []
            assertEq "uma matricula no banco" 1 (length matriculas)

        it "POST /matriculas com aluno inexistente retorna 400" $ do
            (cursoId, turmaId) <- criarTurma
            alunoId <- criarAluno cursoId "Temp"
            runDB $ delete alunoId
            enviarJson "POST" MatriculasR (matriculaJson alunoId turmaId "2026-02-01")
            statusIs 400

        it "POST /matriculas com turma inexistente retorna 400" $ do
            (cursoId, turmaId) <- criarTurma
            alunoId <- criarAluno cursoId "Ana"
            runDB $ delete turmaId
            enviarJson "POST" MatriculasR (matriculaJson alunoId turmaId "2026-02-01")
            statusIs 400

        it "POST /matriculas com data no futuro retorna 400" $ do
            (cursoId, turmaId) <- criarTurma
            alunoId <- criarAluno cursoId "Ana"
            enviarJson "POST" MatriculasR (matriculaJson alunoId turmaId "2999-01-01")
            statusIs 400

        it "POST /matriculas repetida (mesmo aluno e turma) retorna 409" $ do
            (cursoId, turmaId) <- criarTurma
            alunoId <- criarAluno cursoId "Ana"
            enviarJson "POST" MatriculasR (matriculaJson alunoId turmaId "2026-02-01")
            enviarJson "POST" MatriculasR (matriculaJson alunoId turmaId "2026-02-02")
            statusIs 409

        it "GET /matriculas?alunoId= lista so as matriculas do aluno" $ do
            (cursoId, turmaId) <- criarTurma
            ana   <- criarAluno cursoId "Ana"
            bruno <- criarAluno cursoId "Bruno"
            _ <- runDB $ insert $ Matricula ana turmaId (fromGregorian 2026 2 1)
            _ <- runDB $ insert $ Matricula bruno turmaId (fromGregorian 2026 2 2)
            request $ do
                setMethod "GET"
                setUrl MatriculasR
                addGetParam "alunoId" (tshow (fromSqlKey ana))
            statusIs 200
            bodyContains "2026-02-01"
            bodyNotContains "2026-02-02"

        it "GET, PUT e DELETE em /matriculas/#MatriculaId" $ do
            (cursoId, turmaId) <- criarTurma
            alunoId <- criarAluno cursoId "Ana"
            matriculaId <- runDB $ insert $ Matricula alunoId turmaId (fromGregorian 2026 2 1)

            get (MatriculaR matriculaId)
            statusIs 200
            bodyContains "2026-02-01"

            enviarJson "PUT" (MatriculaR matriculaId) (matriculaJson alunoId turmaId "2026-03-01")
            statusIs 200
            bodyContains "2026-03-01"

            request $ setMethod "DELETE" >> setUrl (MatriculaR matriculaId)
            statusIs 204
            restante <- runDB $ count [MatriculaId ==. matriculaId]
            assertEq "matricula removida" 0 restante

        it "DELETE /matriculas/#MatriculaId com notas retorna 409" $ do
            (cursoId, turmaId) <- criarTurma
            alunoId <- criarAluno cursoId "Ana"
            matriculaId <- runDB $ insert $ Matricula alunoId turmaId (fromGregorian 2026 2 1)
            _ <- runDB $ insert $ Nota matriculaId "Prova 1" 8.5
            request $ setMethod "DELETE" >> setUrl (MatriculaR matriculaId)
            statusIs 409

        it "GET /alunos/#AlunoId/matriculas traz turma e disciplina" $ do
            (cursoId, turmaId) <- criarTurma
            alunoId <- criarAluno cursoId "Ana"
            _ <- runDB $ insert $ Matricula alunoId turmaId (fromGregorian 2026 2 1)
            get (AlunoMatriculasR alunoId)
            statusIs 200
            bodyContains "Programação Funcional"
            bodyContains "2026.1"

        it "GET /turmas/#TurmaId/matriculas traz os alunos da turma" $ do
            (cursoId, turmaId) <- criarTurma
            alunoId <- criarAluno cursoId "Ana"
            _ <- runDB $ insert $ Matricula alunoId turmaId (fromGregorian 2026 2 1)
            get (TurmaMatriculasR turmaId)
            statusIs 200
            bodyContains "Ana"

        it "GET /matriculas/#MatriculaId inexistente retorna 404" $ do
            (cursoId, turmaId) <- criarTurma
            alunoId <- criarAluno cursoId "Ana"
            matriculaId <- runDB $ insert $ Matricula alunoId turmaId (fromGregorian 2026 2 1)
            runDB $ delete matriculaId
            get (MatriculaR matriculaId)
            statusIs 404