{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Professor where

import Import
import Network.HTTP.Types (Status, status201, status204, status400, status409)
import qualified Data.Text as T

-- | GET /professores - lista todos os professores em ordem alfabética.
getProfessoresR :: Handler Value
getProfessoresR = do
    professores <- runDB $ selectList [] [Asc ProfessorNome]
    returnJson professores

-- | POST /professores - cadastra um professor a partir do JSON recebido.
postProfessoresR :: Handler Value
postProfessoresR = do
    professor <- requireCheckJsonBody :: Handler Professor
    validarProfessor professor
    resultado <- runDB $ insertUnique professor
    case resultado of
        Nothing ->
            erro status409 "Já existe um professor com esse email"
        Just professorId ->
            sendStatusJSON status201 (Entity professorId professor)

-- | GET /professores/#ProfessorId - busca um professor pelo id.
getProfessorR :: ProfessorId -> Handler Value
getProfessorR professorId = do
    professor <- runDB $ get404 professorId
    returnJson (Entity professorId professor)

-- | PUT /professores/#ProfessorId - substitui os dados de um professor.
putProfessorR :: ProfessorId -> Handler Value
putProfessorR professorId = do
    _ <- runDB $ get404 professorId
    professor <- requireCheckJsonBody :: Handler Professor
    validarProfessor professor
    mesmoEmail <- runDB $ getBy $
        UniqueProfessorEmail (professorEmail professor)
    case mesmoEmail of
        Just (Entity outroId _) | outroId /= professorId ->
            erro status409 "Já existe um professor com esse email"
        _ -> return ()
    runDB $ replace professorId professor
    returnJson (Entity professorId professor)

-- | DELETE /professores/#ProfessorId - remove um professor sem turmas vinculadas.
deleteProfessorR :: ProfessorId -> Handler Value
deleteProfessorR professorId = do
    _ <- runDB $ get404 professorId
    turmas <- runDB $ count [TurmaProfessorId ==. professorId]
    when (turmas > 0) $
        erro status409 "O professor possui turmas vinculadas"
    runDB $ delete professorId
    sendResponseStatus status204 ()

-- | Regras de negócio do professor.
validarProfessor :: Professor -> Handler ()
validarProfessor professor = do
    let erros :: [Text]
        erros =
            [ "O nome do professor é obrigatório"
            | T.null (T.strip (professorNome professor)) ]
            ++
            [ "O email do professor é obrigatório"
            | T.null (T.strip (professorEmail professor)) ]
            ++
            [ "A especialidade do professor é obrigatória"
            | T.null (T.strip (professorEspecialidade professor)) ]
    unless (null erros) $
        sendStatusJSON status400 (object ["erros" .= erros])

-- | Responde com um erro em JSON: {"erro": "..."}.
erro :: Status -> Text -> Handler a
erro status mensagem =
    sendStatusJSON status (object ["erro" .= mensagem])
