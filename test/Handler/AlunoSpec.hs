{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.AlunoSpec (spec) where

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

-- aluno valido pra reaproveitar nos testes
alunoJson :: CursoId -> Value
alunoJson cursoId = object
    [ "nome"           .= ("Ana Souza" :: Text)
    , "email"          .= ("ana@escola.com" :: Text)
    , "ra"             .= ("RA001" :: Text)
    , "dataNascimento" .= ("2000-05-10" :: Text)
    , "cursoId"        .= cursoId
    ]

-- cria curso no banco (aluno precisa de curso)
criarCurso :: Text -> YesodExample App CursoId
criarCurso nome = runDB $ insert $ Curso nome Nothing 3200

spec :: Spec
spec = withApp $ do
    describe "CRUD de Aluno" $ do
        it "POST /alunos cria um aluno (201)" $ do
            cursoId <- criarCurso "Sistemas"
            enviarJson "POST" AlunosR (alunoJson cursoId)
            statusIs 201
            alunos <- runDB $ selectList ([] :: [Filter Aluno]) []
            assertEq "um aluno no banco" 1 (length alunos)

        it "POST /alunos com curso inexistente retorna 400" $ do
            cursoId <- criarCurso "Temporário"
            runDB $ delete cursoId
            enviarJson "POST" AlunosR (alunoJson cursoId)
            statusIs 400

        it "POST /alunos com e-mail inválido retorna 400" $ do
            cursoId <- criarCurso "Sistemas"
            enviarJson "POST" AlunosR $ object
                [ "nome"           .= ("Ana Souza" :: Text)
                , "email"          .= ("ana-sem-arroba" :: Text)
                , "ra"             .= ("RA001" :: Text)
                , "dataNascimento" .= ("2000-05-10" :: Text)
                , "cursoId"        .= cursoId
                ]
            statusIs 400

        it "POST /alunos com data de nascimento no futuro retorna 400" $ do
            cursoId <- criarCurso "Sistemas"
            enviarJson "POST" AlunosR $ object
                [ "nome"           .= ("Ana Souza" :: Text)
                , "email"          .= ("ana@escola.com" :: Text)
                , "ra"             .= ("RA001" :: Text)
                , "dataNascimento" .= ("2999-01-01" :: Text)
                , "cursoId"        .= cursoId
                ]
            statusIs 400

        it "POST /alunos com e-mail repetido retorna 409" $ do
            cursoId <- criarCurso "Sistemas"
            enviarJson "POST" AlunosR (alunoJson cursoId)
            enviarJson "POST" AlunosR $ object
                [ "nome"           .= ("Outra Ana" :: Text)
                , "email"          .= ("ana@escola.com" :: Text)
                , "ra"             .= ("RA002" :: Text)
                , "dataNascimento" .= ("2001-01-01" :: Text)
                , "cursoId"        .= cursoId
                ]
            statusIs 409

        it "POST /alunos com RA repetido retorna 409" $ do
            cursoId <- criarCurso "Sistemas"
            enviarJson "POST" AlunosR (alunoJson cursoId)
            enviarJson "POST" AlunosR $ object
                [ "nome"           .= ("Outra Ana" :: Text)
                , "email"          .= ("outra@escola.com" :: Text)
                , "ra"             .= ("RA001" :: Text)
                , "dataNascimento" .= ("2001-01-01" :: Text)
                , "cursoId"        .= cursoId
                ]
            statusIs 409

        it "GET /alunos?cursoId= lista apenas os alunos do curso" $ do
            cursoA <- criarCurso "Sistemas"
            cursoB <- criarCurso "Direito"
            _ <- runDB $ insert $
                Aluno "Ana Souza" "ana@escola.com" "RA001" (fromGregorian 2000 5 10) cursoA
            _ <- runDB $ insert $
                Aluno "Bruno Lima" "bruno@escola.com" "RA002" (fromGregorian 1999 3 2) cursoB
            request $ do
                setMethod "GET"
                setUrl AlunosR
                addGetParam "cursoId" (tshow (fromSqlKey cursoA))
            statusIs 200
            bodyContains "Ana Souza"
            bodyNotContains "Bruno Lima"

        it "GET, PUT e DELETE em /alunos/#AlunoId" $ do
            cursoId <- criarCurso "Sistemas"
            alunoId <- runDB $ insert $
                Aluno "Ana Souza" "ana@escola.com" "RA001" (fromGregorian 2000 5 10) cursoId

            get (AlunoR alunoId)
            statusIs 200
            bodyContains "Ana Souza"

            enviarJson "PUT" (AlunoR alunoId) $ object
                [ "nome"           .= ("Ana Souza Lima" :: Text)
                , "email"          .= ("ana@escola.com" :: Text)
                , "ra"             .= ("RA001" :: Text)
                , "dataNascimento" .= ("2000-05-10" :: Text)
                , "cursoId"        .= cursoId
                ]
            statusIs 200
            bodyContains "Ana Souza Lima"

            request $ setMethod "DELETE" >> setUrl (AlunoR alunoId)
            statusIs 204
            restante <- runDB $ count [AlunoId ==. alunoId]
            assertEq "aluno removido" 0 restante

        it "DELETE /alunos/#AlunoId com matrículas retorna 409" $ do
            cursoId <- criarCurso "Sistemas"
            alunoId <- runDB $ insert $
                Aluno "Ana Souza" "ana@escola.com" "RA001" (fromGregorian 2000 5 10) cursoId
            professorId <- runDB $ insert $ Professor "Carlos" "carlos@escola.com" "Haskell"
            disciplinaId <- runDB $ insert $ Disciplina "Programação Funcional" 80 cursoId
            turmaId <- runDB $ insert $ Turma "T1" "2026.1" disciplinaId professorId
            _ <- runDB $ insert $ Matricula alunoId turmaId (fromGregorian 2026 2 1)

            request $ setMethod "DELETE" >> setUrl (AlunoR alunoId)
            statusIs 409

        it "GET /alunos/#AlunoId inexistente retorna 404" $ do
            cursoId <- criarCurso "Sistemas"
            alunoId <- runDB $ insert $
                Aluno "Temporário" "tmp@escola.com" "RA999" (fromGregorian 2000 1 1) cursoId
            runDB $ delete alunoId
            get (AlunoR alunoId)
            statusIs 404