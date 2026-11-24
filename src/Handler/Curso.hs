{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Curso where

import Import
import Network.HTTP.Types (Status, status201, status204, status400, status409)
import qualified Data.Text as T

-- | GET /cursos — lista todos os cursos, em ordem alfabética.
getCursosR :: Handler Value
getCursosR = do
    cursos <- runDB $ selectList [] [Asc CursoNome]
    returnJson cursos

-- | POST /cursos — cadastra um curso a partir do JSON recebido.
postCursosR :: Handler Value
postCursosR = do
    curso <- requireCheckJsonBody :: Handler Curso
    validarCurso curso
    resultado <- runDB $ insertUnique curso
    case resultado of
        Nothing      -> erro status409 "Já existe um curso com esse nome"
        Just cursoId -> sendStatusJSON status201 (Entity cursoId curso)

-- | GET /cursos/#CursoId — busca um curso pelo id.
getCursoR :: CursoId -> Handler Value
getCursoR cursoId = do
    curso <- runDB $ get404 cursoId
    returnJson (Entity cursoId curso)

-- | PUT /cursos/#CursoId — substitui os dados de um curso existente.
putCursoR :: CursoId -> Handler Value
putCursoR cursoId = do
    _ <- runDB $ get404 cursoId
    curso <- requireCheckJsonBody :: Handler Curso
    validarCurso curso
    mesmoNome <- runDB $ getBy $ UniqueCursoNome (cursoNome curso)
    case mesmoNome of
        Just (Entity outroId _) | outroId /= cursoId ->
            erro status409 "Já existe um curso com esse nome"
        _ -> return ()
    runDB $ replace cursoId curso
    returnJson (Entity cursoId curso)

-- | DELETE /cursos/#CursoId — remove um curso sem disciplinas nem alunos.
deleteCursoR :: CursoId -> Handler Value
deleteCursoR cursoId = do
    _ <- runDB $ get404 cursoId
    disciplinas <- runDB $ count [DisciplinaCursoId ==. cursoId]
    alunos      <- runDB $ count [AlunoCursoId ==. cursoId]
    when (disciplinas + alunos > 0) $
        erro status409 "O curso possui disciplinas ou alunos vinculados"
    runDB $ delete cursoId
    sendResponseStatus status204 ()

-- | Regras de negócio do curso. Interrompe a requisição com 400 se houver erro.
validarCurso :: Curso -> Handler ()
validarCurso curso = do
    let erros :: [Text]
        erros =
            [ "O nome do curso é obrigatório"
            | T.null (T.strip (cursoNome curso)) ]
            ++
            [ "A carga horária deve ser maior que zero"
            | cursoCargaHoraria curso <= 0 ]
    unless (null erros) $
        sendStatusJSON status400 (object ["erros" .= erros])

-- | Responde com um erro em JSON: {"erro": "..."}.
erro :: Status -> Text -> Handler a
erro status mensagem = sendStatusJSON status (object ["erro" .= mensagem])