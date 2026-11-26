{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Turma where

import Import
import qualified Data.Text as T

-- | GET /turmas - lista todas as turmas em ordem alfabética pelo código.
getTurmasR :: Handler Value
getTurmasR = do
    turmas <- runDB $ selectList [] [Asc TurmaCodigo]
    returnJson turmas

-- | POST /turmas - cadastra uma turma a partir do JSON recebido.
postTurmasR :: Handler Value
postTurmasR = do
    turma <- requireCheckJsonBody :: Handler Turma
    validarTurma turma
    resultado <- runDB $ insertUnique turma
    case resultado of
        Nothing ->
            erro status409 "Já existe uma turma com esse código"
        Just turmaId ->
            sendStatusJSON status201 (Entity turmaId turma)

-- | GET /turmas/#TurmaId - busca uma turma pelo id.
getTurmaR :: TurmaId -> Handler Value
getTurmaR turmaId = do
    turma <- runDB $ get404 turmaId
    returnJson (Entity turmaId turma)

-- | PUT /turmas/#TurmaId - substitui os dados de uma turma.
putTurmaR :: TurmaId -> Handler Value
putTurmaR turmaId = do
    _ <- runDB $ get404 turmaId
    turma <- requireCheckJsonBody :: Handler Turma
    validarTurma turma
    mesmoCodigo <- runDB $ getBy $ UniqueTurmaCodigo (turmaCodigo turma)
    case mesmoCodigo of
        Just (Entity outroId _) | outroId /= turmaId ->
            erro status409 "Já existe uma turma com esse código"
        _ -> return ()
    runDB $ replace turmaId turma
    returnJson (Entity turmaId turma)

-- | DELETE /turmas/#TurmaId - remove uma turma sem matrículas vinculadas.
deleteTurmaR :: TurmaId -> Handler Value
deleteTurmaR turmaId = do
    _ <- runDB $ get404 turmaId
    matriculas <- runDB $ count [MatriculaTurmaId ==. turmaId]
    when (matriculas > 0) $
        erro status409 "A turma possui matrículas vinculadas"
    runDB $ delete turmaId
    sendResponseStatus status204 ()

-- | Regras de negócio da turma.
validarTurma :: Turma -> Handler ()
validarTurma turma = do
    let erros :: [Text]
        erros =
            [ "O código da turma é obrigatório"
            | T.null (T.strip (turmaCodigo turma)) ]
            ++
            [ "O semestre da turma é obrigatório"
            | T.null (T.strip (turmaSemestre turma)) ]
    unless (null erros) $
        sendStatusJSON status400 (object ["erros" .= erros])
    disciplina <- runDB $ get (turmaDisciplinaId turma)
    when (isNothing disciplina) $
        erro status400 "A disciplina informada não existe"
    professor <- runDB $ get (turmaProfessorId turma)
    when (isNothing professor) $
        erro status400 "O professor informado não existe"

-- | Responde com um erro em JSON: {"erro": "..."}.
erro :: Status -> Text -> Handler a
erro status mensagem =
    sendStatusJSON status (object ["erro" .= mensagem])
