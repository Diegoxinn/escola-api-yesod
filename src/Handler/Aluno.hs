{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Aluno
    ( getAlunosR
    , postAlunosR
    , getAlunoR
    , putAlunoR
    , deleteAlunoR
    ) where

import Import
import Data.Char (isSpace)
import Data.Time (getCurrentTime, utctDay)
import Network.HTTP.Types (Status, status201, status204, status400, status409)
import qualified Data.Text as T

-- lista os alunos (ordem alfabetica)
-- da pra filtrar por curso: /alunos?cursoId=1
getAlunosR :: Handler Value
getAlunosR = do
    cursoParam <- lookupGetParam "cursoId"
    filtros <- case cursoParam of
        Nothing    -> return []
        Just texto -> case fromPathPiece texto of
            Nothing      -> erro status400 "O parâmetro cursoId é inválido"
            Just cursoId -> return [AlunoCursoId ==. cursoId]
    alunos <- runDB $ selectList filtros [Asc AlunoNome]
    returnJson alunos

-- cadastra aluno
postAlunosR :: Handler Value
postAlunosR = do
    aluno <- requireCheckJsonBody :: Handler Aluno
    validarAluno aluno
    verificarDuplicidade Nothing aluno
    alunoId <- runDB $ insert aluno
    sendStatusJSON status201 (Entity alunoId aluno)

-- busca aluno pelo id
getAlunoR :: AlunoId -> Handler Value
getAlunoR alunoId = do
    aluno <- runDB $ get404 alunoId
    returnJson (Entity alunoId aluno)

-- atualiza aluno (troca tudo)
putAlunoR :: AlunoId -> Handler Value
putAlunoR alunoId = do
    _ <- runDB $ get404 alunoId
    aluno <- requireCheckJsonBody :: Handler Aluno
    validarAluno aluno
    verificarDuplicidade (Just alunoId) aluno
    runDB $ replace alunoId aluno
    returnJson (Entity alunoId aluno)

-- deleta aluno, mas so se nao tiver matricula
deleteAlunoR :: AlunoId -> Handler Value
deleteAlunoR alunoId = do
    _ <- runDB $ get404 alunoId
    matriculas <- runDB $ count [MatriculaAlunoId ==. alunoId]
    when (matriculas > 0) $
        erro status409 "O aluno possui matrículas vinculadas"
    runDB $ delete alunoId
    sendResponseStatus status204 ()

-- validacoes, se der erro ja devolve 400
validarAluno :: Aluno -> Handler ()
validarAluno aluno = do
    hoje  <- utctDay <$> liftIO getCurrentTime
    curso <- runDB $ get (alunoCursoId aluno)
    let erros :: [Text]
        erros =
            [ "O nome do aluno é obrigatório"
            | T.null (T.strip (alunoNome aluno)) ]
            ++
            [ "O e-mail é inválido"
            | not (emailValido (alunoEmail aluno)) ]
            ++
            [ "O RA é obrigatório"
            | T.null (T.strip (alunoRa aluno)) ]
            ++
            [ "A data de nascimento deve estar no passado"
            | alunoDataNascimento aluno >= hoje ]
            ++
            [ "O curso informado não existe"
            | isNothing curso ]
    unless (null erros) $
        sendStatusJSON status400 (object ["erros" .= erros])

-- email e ra nao podem repetir (409)
-- no PUT passa o id do proprio aluno senao ele conflita com ele mesmo
verificarDuplicidade :: Maybe AlunoId -> Aluno -> Handler ()
verificarDuplicidade atual aluno = do
    porEmail <- runDB $ getBy $ UniqueAlunoEmail (alunoEmail aluno)
    porRa    <- runDB $ getBy $ UniqueAlunoRa (alunoRa aluno)
    when (pertenceAOutro porEmail) $
        erro status409 "Já existe um aluno com esse e-mail"
    when (pertenceAOutro porRa) $
        erro status409 "Já existe um aluno com esse RA"
  where
    pertenceAOutro (Just (Entity outroId _)) = Just outroId /= atual
    pertenceAOutro Nothing                   = False

-- checagem basica de email, tipo a@b.com
emailValido :: Text -> Bool
emailValido email =
    case T.splitOn "@" email of
        [local, dominio] ->
            not (T.null local)
            && T.isInfixOf "." dominio
            && not (T.isPrefixOf "." dominio)
            && not (T.isSuffixOf "." dominio)
            && not (T.any isSpace email)
        _ -> False

-- devolve o erro em json
erro :: Status -> Text -> Handler a
erro status mensagem = sendStatusJSON status (object ["erro" .= mensagem])