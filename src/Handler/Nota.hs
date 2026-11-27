{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Nota
    ( getNotasR
    , postNotasR
    , getNotaR
    , putNotaR
    , deleteNotaR
    , getMatriculaNotasR
    , getAlunoBoletimR
    ) where

import Import
import Network.HTTP.Types (Status, status201, status204, status400)
import qualified Data.Text as T

-- lista as notas
-- filtro opcional: /notas?matriculaId=1
getNotasR :: Handler Value
getNotasR = do
    matriculaParam <- lerParam "matriculaId"
    let filtros = [ NotaMatriculaId ==. m | Just m <- [matriculaParam] ]
    notas <- runDB $ selectList filtros [Asc NotaId]
    returnJson notas

-- lanca uma nota
postNotasR :: Handler Value
postNotasR = do
    nota <- requireCheckJsonBody :: Handler Nota
    validarNota nota
    notaId <- runDB $ insert nota
    sendStatusJSON status201 (Entity notaId nota)

-- busca nota pelo id
getNotaR :: NotaId -> Handler Value
getNotaR notaId = do
    nota <- runDB $ get404 notaId
    returnJson (Entity notaId nota)

-- atualiza nota (troca tudo)
putNotaR :: NotaId -> Handler Value
putNotaR notaId = do
    _ <- runDB $ get404 notaId
    nota <- requireCheckJsonBody :: Handler Nota
    validarNota nota
    runDB $ replace notaId nota
    returnJson (Entity notaId nota)

-- deleta nota
deleteNotaR :: NotaId -> Handler Value
deleteNotaR notaId = do
    _ <- runDB $ get404 notaId
    runDB $ delete notaId
    sendResponseStatus status204 ()

-- GET /matriculas/1/notas
-- notas de uma matricula e a media delas
getMatriculaNotasR :: MatriculaId -> Handler Value
getMatriculaNotasR matriculaId = do
    _ <- runDB $ get404 matriculaId
    notas <- runDB $ selectList [NotaMatriculaId ==. matriculaId] [Asc NotaId]
    returnJson $ object
        [ "notas" .= notas
        , "media" .= mediaDe (map (notaValor . entityVal) notas)
        ]

-- GET /alunos/1/boletim
-- junta aluno, matriculas, turmas, disciplinas e notas
-- e calcula a media de cada disciplina e a media geral
getAlunoBoletimR :: AlunoId -> Handler Value
getAlunoBoletimR alunoId = do
    aluno <- runDB $ get404 alunoId
    boletim <- runDB $ do
        matriculas <- selectList [MatriculaAlunoId ==. alunoId] [Asc MatriculaDataMatricula]
        forM matriculas $ \(Entity matriculaId matricula) -> do
            let turmaId = matriculaTurmaId matricula
            turma <- getJust turmaId
            let disciplinaId = turmaDisciplinaId turma
            disciplina <- getJust disciplinaId
            notas <- selectList [NotaMatriculaId ==. matriculaId] [Asc NotaId]
            let media = mediaDe (map (notaValor . entityVal) notas)
            return
                ( media
                , object
                    [ "matricula"  .= Entity matriculaId matricula
                    , "turma"      .= Entity turmaId turma
                    , "disciplina" .= Entity disciplinaId disciplina
                    , "notas"      .= notas
                    , "media"      .= media
                    ]
                )
    -- media geral so conta as disciplinas que ja tem nota
    let medias = [ m | (Just m, _) <- boletim ]
    returnJson $ object
        [ "aluno"       .= Entity alunoId aluno
        , "disciplinas" .= map snd boletim
        , "mediaGeral"  .= mediaDe medias
        ]

-- validacoes, se der erro ja devolve 400
validarNota :: Nota -> Handler ()
validarNota nota = do
    matricula <- runDB $ get (notaMatriculaId nota)
    let erros :: [Text]
        erros =
            [ "A descrição da nota é obrigatória"
            | T.null (T.strip (notaDescricao nota)) ]
            ++
            [ "O valor da nota deve estar entre 0 e 10"
            | notaValor nota < 0 || notaValor nota > 10 ]
            ++
            [ "A matrícula informada não existe"
            | isNothing matricula ]
    unless (null erros) $
        sendStatusJSON status400 (object ["erros" .= erros])

-- media simples, arredondada em 2 casas (Nothing se nao tem nota)
mediaDe :: [Double] -> Maybe Double
mediaDe [] = Nothing
mediaDe valores = Just (arredondar (sum valores / fromIntegral (length valores)))
  where
    arredondar x = fromIntegral (round (x * 100) :: Int) / 100

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