{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | CRUD do endereço do aluno.
-- O cliente informa só CEP, número e complemento. O restante do endereço
-- (logradouro, bairro, cidade e UF) vem da API externa ViaCEP.
module Handler.Endereco
    ( getEnderecoR
    , postEnderecoR
    , putEnderecoR
    , deleteEnderecoR
    ) where

import Import
import Service.ViaCep
import Data.Aeson (withObject, (.:), (.:?))
import qualified Data.Text as T

-- | Corpo JSON aceito no POST e no PUT. Exemplo:
data EnderecoEntrada = EnderecoEntrada
    { entradaCep         :: Text
    , entradaNumero      :: Text
    , entradaComplemento :: Maybe Text
    }

instance FromJSON EnderecoEntrada where
    parseJSON = withObject "EnderecoEntrada" $ \o -> EnderecoEntrada
        <$> o .:  "cep"
        <*> o .:  "numero"
        <*> o .:? "complemento"   -- .:? = campo opcional

-- | GET /alunos/#AlunoId/endereco — mostra o endereço do aluno.
getEnderecoR :: AlunoId -> Handler Value
getEnderecoR alunoId = do
    endereco <- buscarEnderecoDoAluno alunoId
    returnJson endereco

-- | POST /alunos/#AlunoId/endereco — cadastra o endereço do aluno
-- consultando o CEP no ViaCEP.
postEnderecoR :: AlunoId -> Handler Value
postEnderecoR alunoId = do
    _ <- runDB $ get404 alunoId
    entrada <- requireCheckJsonBody :: Handler EnderecoEntrada
    validarEntrada entrada
    jaExiste <- runDB $ getBy $ UniqueEnderecoAluno alunoId
    case jaExiste of
        Just _  -> erroJson status409
                    "Este aluno já possui endereço. Use PUT para alterar."
        Nothing -> return ()
    endereco   <- montarEndereco alunoId entrada
    enderecoId <- runDB $ insert endereco
    sendStatusJSON status201 (Entity enderecoId endereco)

-- | PUT /alunos/#AlunoId/endereco — troca o endereço do aluno
-- (consulta o novo CEP no ViaCEP).
putEnderecoR :: AlunoId -> Handler Value
putEnderecoR alunoId = do
    Entity enderecoId _ <- buscarEnderecoDoAluno alunoId
    entrada <- requireCheckJsonBody :: Handler EnderecoEntrada
    validarEntrada entrada
    endereco <- montarEndereco alunoId entrada
    runDB $ replace enderecoId endereco
    returnJson (Entity enderecoId endereco)

-- | DELETE /alunos/#AlunoId/endereco — remove o endereço do aluno.
deleteEnderecoR :: AlunoId -> Handler Value
deleteEnderecoR alunoId = do
    Entity enderecoId _ <- buscarEnderecoDoAluno alunoId
    runDB $ delete enderecoId
    sendResponseStatus status204 ()

-- | Junta o que o cliente mandou com o que o ViaCEP devolveu.
-- É aqui que os dados da API externa são usados pelo sistema.
montarEndereco :: AlunoId -> EnderecoEntrada -> Handler Endereco
montarEndereco alunoId entrada = do
    resultado <- buscarCep (entradaCep entrada)
    case resultado of
        Left falha   -> responderErroViaCep falha
        Right viaCep -> return Endereco
            { enderecoAlunoId     = alunoId
            , enderecoCep         = viaCepCep viaCep
            , enderecoLogradouro  = viaCepLogradouro viaCep
            , enderecoNumero      = entradaNumero entrada
            , enderecoComplemento = entradaComplemento entrada
            , enderecoBairro      = viaCepBairro viaCep
            , enderecoCidade      = viaCepCidade viaCep
            , enderecoUf          = viaCepUf viaCep
            }

-- | Busca o endereço de um aluno. Responde 404 se o aluno não existe
-- ou se ainda não tem endereço cadastrado.
buscarEnderecoDoAluno :: AlunoId -> Handler (Entity Endereco)
buscarEnderecoDoAluno alunoId = do
    _ <- runDB $ get404 alunoId
    resultado <- runDB $ getBy $ UniqueEnderecoAluno alunoId
    case resultado of
        Nothing       -> erroJson status404
                            "Este aluno ainda não possui endereço cadastrado"
        Just endereco -> return endereco

-- | Validação dos dados recebidos. Interrompe com 400 se houver erro.
validarEntrada :: EnderecoEntrada -> Handler ()
validarEntrada entrada = do
    let erros :: [Text]
        erros =
            [ "O CEP deve ter 8 dígitos"
            | T.length (limparCep (entradaCep entrada)) /= 8 ]
            ++
            [ "O número é obrigatório"
            | T.null (T.strip (entradaNumero entrada)) ]
    unless (null erros) $
        sendStatusJSON status400 (object ["erros" .= erros])

-- | Responde com um erro em JSON: {"erro": "..."}.
erroJson :: Status -> Text -> Handler a
erroJson status mensagem = sendStatusJSON status (object ["erro" .= mensagem])
