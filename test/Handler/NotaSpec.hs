{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.NotaSpec (spec) where

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

-- cria curso, professor, disciplina e turma
criarTurma :: YesodExample App (CursoId, TurmaId)
criarTurma = runDB $ do
    cursoId      <- insert $ Curso "Sistemas" Nothing 3200
    professorId  <- insert $ Professor "Carlos" "carlos@escola.com" "Haskell"
    disciplinaId <- insert $ Disciplina "Programação Funcional" 80 cursoId
    turmaId      <- insert $ Turma "T1" "2026.1" disciplinaId professorId
    return (cursoId, turmaId)

-- cria aluno e ja matricula na turma
matricular :: CursoId -> TurmaId -> Text -> YesodExample App (AlunoId, MatriculaId)
matricular cursoId turmaId nome = runDB $ do
    alunoId <- insert $
        Aluno nome (nome <> "@escola.com") ("RA-" <> nome) (fromGregorian 2000 5 10) cursoId
    matriculaId <- insert $ Matricula alunoId turmaId (fromGregorian 2026 2 1)
    return (alunoId, matriculaId)

-- json de nota pra reaproveitar
notaJson :: MatriculaId -> Text -> Double -> Value
notaJson matriculaId descricao valor = object
    [ "matriculaId" .= matriculaId
    , "descricao"   .= descricao
    , "valor"       .= valor
    ]

spec :: Spec
spec = withApp $ do
    describe "CRUD de Nota" $ do
        it "POST /notas lanca uma nota (201)" $ do
            (cursoId, turmaId) <- criarTurma
            (_, matriculaId) <- matricular cursoId turmaId "Ana"
            enviarJson "POST" NotasR (notaJson matriculaId "Prova 1" 8.5)
            statusIs 201
            notas <- runDB $ selectList ([] :: [Filter Nota]) []
            assertEq "uma nota no banco" 1 (length notas)

        it "POST /notas com matricula inexistente retorna 400" $ do
            (cursoId, turmaId) <- criarTurma
            (_, matriculaId) <- matricular cursoId turmaId "Ana"
            runDB $ delete matriculaId
            enviarJson "POST" NotasR (notaJson matriculaId "Prova 1" 8.5)
            statusIs 400

        it "POST /notas com valor acima de 10 retorna 400" $ do
            (cursoId, turmaId) <- criarTurma
            (_, matriculaId) <- matricular cursoId turmaId "Ana"
            enviarJson "POST" NotasR (notaJson matriculaId "Prova 1" 11)
            statusIs 400

        it "POST /notas com valor negativo retorna 400" $ do
            (cursoId, turmaId) <- criarTurma
            (_, matriculaId) <- matricular cursoId turmaId "Ana"
            enviarJson "POST" NotasR (notaJson matriculaId "Prova 1" (-1))
            statusIs 400

        it "POST /notas com descricao vazia retorna 400" $ do
            (cursoId, turmaId) <- criarTurma
            (_, matriculaId) <- matricular cursoId turmaId "Ana"
            enviarJson "POST" NotasR (notaJson matriculaId "" 8)
            statusIs 400

        it "GET /notas?matriculaId= lista so as notas da matricula" $ do
            (cursoId, turmaId) <- criarTurma
            (_, matAna)   <- matricular cursoId turmaId "Ana"
            (_, matBruno) <- matricular cursoId turmaId "Bruno"
            _ <- runDB $ insert $ Nota matAna "Prova da Ana" 8
            _ <- runDB $ insert $ Nota matBruno "Prova do Bruno" 6
            request $ do
                setMethod "GET"
                setUrl NotasR
                addGetParam "matriculaId" (tshow (fromSqlKey matAna))
            statusIs 200
            bodyContains "Prova da Ana"
            bodyNotContains "Prova do Bruno"

        it "GET, PUT e DELETE em /notas/#NotaId" $ do
            (cursoId, turmaId) <- criarTurma
            (_, matriculaId) <- matricular cursoId turmaId "Ana"
            notaId <- runDB $ insert $ Nota matriculaId "Prova 1" 8.5

            get (NotaR notaId)
            statusIs 200
            bodyContains "Prova 1"

            enviarJson "PUT" (NotaR notaId) (notaJson matriculaId "Prova 1 final" 9)
            statusIs 200
            bodyContains "Prova 1 final"

            request $ setMethod "DELETE" >> setUrl (NotaR notaId)
            statusIs 204
            restante <- runDB $ count [NotaId ==. notaId]
            assertEq "nota removida" 0 restante

        it "GET /matriculas/#MatriculaId/notas traz as notas e a media" $ do
            (cursoId, turmaId) <- criarTurma
            (_, matriculaId) <- matricular cursoId turmaId "Ana"
            _ <- runDB $ insert $ Nota matriculaId "Prova 1" 8
            _ <- runDB $ insert $ Nota matriculaId "Prova 2" 7
            get (MatriculaNotasR matriculaId)
            statusIs 200
            bodyContains "Prova 1"
            bodyContains "7.5"

        it "GET /alunos/#AlunoId/boletim traz disciplina, notas e medias" $ do
            (cursoId, turmaId) <- criarTurma
            (alunoId, matriculaId) <- matricular cursoId turmaId "Ana"
            _ <- runDB $ insert $ Nota matriculaId "Prova 1" 8
            _ <- runDB $ insert $ Nota matriculaId "Prova 2" 7
            get (AlunoBoletimR alunoId)
            statusIs 200
            bodyContains "Programação Funcional"
            bodyContains "mediaGeral"
            bodyContains "7.5"

        it "GET /notas/#NotaId inexistente retorna 404" $ do
            (cursoId, turmaId) <- criarTurma
            (_, matriculaId) <- matricular cursoId turmaId "Ana"
            notaId <- runDB $ insert $ Nota matriculaId "Prova 1" 8
            runDB $ delete notaId
            get (NotaR notaId)
            statusIs 404