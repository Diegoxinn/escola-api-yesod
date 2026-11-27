{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Disciplina where

import Import
import Network.HTTP.Types (Status, status201, status204, status400, status409)
import qualified Data.Text as T

-- | GET /disciplinas - lista todas as disciplinas em ordem alfabética.
getDisciplinasR :: Handler Value
getDisciplinasR = do
    disciplinas <- runDB $ selectList [] [Asc DisciplinaNome]
    returnJson disciplinas

-- | POST /disciplinas - cadastra uma disciplina a partir do JSON recebido.
postDisciplinasR :: Handler Value
postDisciplinasR = do
    disciplina <- requireCheckJsonBody :: Handler Disciplina
    validarDisciplina disciplina
    disciplinaId <- runDB $ insert disciplina
    sendStatusJSON status201 (Entity disciplinaId disciplina)

-- | GET /disciplinas/#DisciplinaId - busca uma disciplina pelo id.
getDisciplinaR :: DisciplinaId -> Handler Value
getDisciplinaR disciplinaId = do
    disciplina <- runDB $ get404 disciplinaId
    returnJson (Entity disciplinaId disciplina)

-- | PUT /disciplinas/#DisciplinaId - substitui os dados de uma disciplina.
putDisciplinaR :: DisciplinaId -> Handler Value
putDisciplinaR disciplinaId = do
    _ <- runDB $ get404 disciplinaId
    disciplina <- requireCheckJsonBody :: Handler Disciplina
    validarDisciplina disciplina
    runDB $ replace disciplinaId disciplina
    returnJson (Entity disciplinaId disciplina)

-- | DELETE /disciplinas/#DisciplinaId - remove uma disciplina sem turmas vinculadas.
deleteDisciplinaR :: DisciplinaId -> Handler Value
deleteDisciplinaR disciplinaId = do
    _ <- runDB $ get404 disciplinaId
    turmas <- runDB $ count [TurmaDisciplinaId ==. disciplinaId]
    when (turmas > 0) $
        erro status409 "A disciplina possui turmas vinculadas"
    runDB $ delete disciplinaId
    sendResponseStatus status204 ()

-- | Regras de negócio da disciplina.
validarDisciplina :: Disciplina -> Handler ()
validarDisciplina disciplina = do
    let erros :: [Text]
        erros =
            [ "O nome da disciplina é obrigatório"
            | T.null (T.strip (disciplinaNome disciplina)) ]
            ++
            [ "A carga horária deve ser maior que zero"
            | disciplinaCargaHoraria disciplina <= 0 ]
    unless (null erros) $
        sendStatusJSON status400 (object ["erros" .= erros])
    curso <- runDB $ get (disciplinaCursoId disciplina)
    when (isNothing curso) $
        erro status400 "O curso informado não existe"

-- | Responde com um erro em JSON: {"erro": "..."}.
erro :: Status -> Text -> Handler a
erro status mensagem =
    sendStatusJSON status (object ["erro" .= mensagem])
