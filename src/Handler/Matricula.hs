{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Matricula
    ( getMatriculasR
    , postMatriculasR
    , getMatriculaR
    , putMatriculaR
    , deleteMatriculaR
    , getAlunoMatriculasR
    , getTurmaMatriculasR
    ) where

import Import
import Data.Time (getCurrentTime, utctDay)
import Network.HTTP.Types (Status, status201, status204, status400, status409)

-- lista as matriculas
-- filtros opcionais: /matriculas?alunoId=1 e /matriculas?turmaId=2
getMatriculasR :: Handler Value
getMatriculasR = do
    alunoParam <- lerParam "alunoId"
    turmaParam <- lerParam "turmaId"
    let filtros =
            [ MatriculaAlunoId ==. a | Just a <- [alunoParam] ]
            ++
            [ MatriculaTurmaId ==. t | Just t <- [turmaParam] ]
    matriculas <- runDB $ selectList filtros [Asc MatriculaDataMatricula]
    returnJson matriculas

-- matricula um aluno em uma turma
postMatriculasR :: Handler Value
postMatriculasR = do
    matricula <- requireCheckJsonBody :: Handler Matricula
    validarMatricula matricula
    verificarDuplicidade Nothing matricula
    matriculaId <- runDB $ insert matricula
    sendStatusJSON status201 (Entity matriculaId matricula)

-- busca matricula pelo id
getMatriculaR :: MatriculaId -> Handler Value
getMatriculaR matriculaId = do
    matricula <- runDB $ get404 matriculaId
    returnJson (Entity matriculaId matricula)

-- atualiza matricula (troca tudo)
putMatriculaR :: MatriculaId -> Handler Value
putMatriculaR matriculaId = do
    _ <- runDB $ get404 matriculaId
    matricula <- requireCheckJsonBody :: Handler Matricula
    validarMatricula matricula
    verificarDuplicidade (Just matriculaId) matricula
    runDB $ replace matriculaId matricula
    returnJson (Entity matriculaId matricula)

-- deleta matricula, mas so se nao tiver nota lancada
deleteMatriculaR :: MatriculaId -> Handler Value
deleteMatriculaR matriculaId = do
    _ <- runDB $ get404 matriculaId
    notas <- runDB $ count [NotaMatriculaId ==. matriculaId]
    when (notas > 0) $
        erro status409 "A matrícula possui notas lançadas"
    runDB $ delete matriculaId
    sendResponseStatus status204 ()

-- GET /alunos/1/matriculas
-- matriculas do aluno, ja com a turma e a disciplina de cada uma
getAlunoMatriculasR :: AlunoId -> Handler Value
getAlunoMatriculasR alunoId = do
    _ <- runDB $ get404 alunoId
    resultado <- runDB $ do
        matriculas <- selectList [MatriculaAlunoId ==. alunoId] [Asc MatriculaDataMatricula]
        forM matriculas $ \(Entity matriculaId matricula) -> do
            let turmaId = matriculaTurmaId matricula
            turma <- getJust turmaId
            let disciplinaId = turmaDisciplinaId turma
            disciplina <- getJust disciplinaId
            return $ object
                [ "matricula"  .= Entity matriculaId matricula
                , "turma"      .= Entity turmaId turma
                , "disciplina" .= Entity disciplinaId disciplina
                ]
    returnJson (resultado :: [Value])

-- GET /turmas/1/matriculas
-- matriculas da turma, ja com os dados do aluno
getTurmaMatriculasR :: TurmaId -> Handler Value
getTurmaMatriculasR turmaId = do
    _ <- runDB $ get404 turmaId
    resultado <- runDB $ do
        matriculas <- selectList [MatriculaTurmaId ==. turmaId] [Asc MatriculaDataMatricula]
        forM matriculas $ \(Entity matriculaId matricula) -> do
            let alunoId = matriculaAlunoId matricula
            aluno <- getJust alunoId
            return $ object
                [ "matricula" .= Entity matriculaId matricula
                , "aluno"     .= Entity alunoId aluno
                ]
    returnJson (resultado :: [Value])

-- validacoes, se der erro ja devolve 400
validarMatricula :: Matricula -> Handler ()
validarMatricula matricula = do
    hoje  <- utctDay <$> liftIO getCurrentTime
    aluno <- runDB $ get (matriculaAlunoId matricula)
    turma <- runDB $ get (matriculaTurmaId matricula)
    let erros :: [Text]
        erros =
            [ "O aluno informado não existe"
            | isNothing aluno ]
            ++
            [ "A turma informada não existe"
            | isNothing turma ]
            ++
            [ "A data da matrícula não pode estar no futuro"
            | matriculaDataMatricula matricula > hoje ]
    unless (null erros) $
        sendStatusJSON status400 (object ["erros" .= erros])

-- o aluno nao pode estar matriculado duas vezes na mesma turma (409)
-- no PUT passa o id da propria matricula senao ela conflita com ela mesma
verificarDuplicidade :: Maybe MatriculaId -> Matricula -> Handler ()
verificarDuplicidade atual matricula = do
    existente <- runDB $ getBy $
        UniqueMatricula (matriculaAlunoId matricula) (matriculaTurmaId matricula)
    case existente of
        Just (Entity outroId _) | Just outroId /= atual ->
            erro status409 "O aluno já está matriculado nessa turma"
        _ -> return ()

-- le um parametro de id da query string, se vier invalido da 400
lerParam :: PathPiece a => Text -> Handler (Maybe a)
lerParam nome = do
    valor <- lookupGetParam nome
    case valor of
        Nothing    -> return Nothing
        Just texto -> case fromPathPiece texto of
            Nothing -> erro status400 ("O parâmetro " <> nome <> " é inválido")
            Just x  -> return (Just x)

-- devolve o erro em json
erro :: Status -> Text -> Handler a
erro status mensagem = sendStatusJSON status (object ["erro" .= mensagem])