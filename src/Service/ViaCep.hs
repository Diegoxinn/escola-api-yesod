{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Comunicação com a API externa ViaCEP (https://viacep.com.br).
-- Recebe um CEP, faz uma requisição HTTP para o ViaCEP e converte o JSON
-- da resposta em um tipo Haskell que o resto do sistema pode usar.
module Service.ViaCep
    ( EnderecoViaCep (..)
    , ErroViaCep (..)
    , buscarCep
    , limparCep
    , responderErroViaCep
    ) where

import Import
import Data.Aeson (decode, withObject, (.:))
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as HTTP

-- | Os campos da resposta do ViaCEP que o sistema utiliza.
data EnderecoViaCep = EnderecoViaCep
    { viaCepCep        :: Text
    , viaCepLogradouro :: Text
    , viaCepBairro     :: Text
    , viaCepCidade     :: Text
    , viaCepUf         :: Text
    } deriving Show

-- | O que pode dar errado ao consultar o ViaCEP.
data ErroViaCep
    = CepInvalido          -- ^ o CEP não tem 8 dígitos
    | CepNaoEncontrado     -- ^ o ViaCEP respondeu {"erro": "true"}
    | ServicoIndisponivel  -- ^ sem internet, demora demais ou ViaCEP fora do ar
    deriving Show

-- | Como ler o JSON do ViaCEP.
-- Campos que não usamos (ibge, ddd...) são ignorados.
instance FromJSON EnderecoViaCep where
    parseJSON = withObject "EnderecoViaCep" $ \o -> EnderecoViaCep
        <$> o .: "cep"
        <*> o .: "logradouro"
        <*> o .: "bairro"
        <*> o .: "localidade"
        <*> o .: "uf"

-- | Como devolver o endereço no JSON da nossa API (nomes em português).
instance ToJSON EnderecoViaCep where
    toJSON e = object
        [ "cep"        .= viaCepCep e
        , "logradouro" .= viaCepLogradouro e
        , "bairro"     .= viaCepBairro e
        , "cidade"     .= viaCepCidade e
        , "uf"         .= viaCepUf e
        ]

-- | Deixa só os dígitos do CEP: "01001-000" vira "01001000".
limparCep :: Text -> Text
limparCep = T.filter C.isDigit

-- | Consulta um CEP no ViaCEP.
-- Devolve Right com o endereço, ou Left dizendo o que deu errado.
buscarCep :: Text -> Handler (Either ErroViaCep EnderecoViaCep)
buscarCep cep
    -- 1) Valida antes de gastar uma requisição com um CEP impossível.
    | T.length cepLimpo /= 8 = return (Left CepInvalido)
    | otherwise = do
        manager    <- getsYesod appHttpManager
        requisicao <- liftIO $ HTTP.parseRequest (urlViaCep cepLimpo)
        -- 2) Desiste depois de 5 segundos (o valor é em microssegundos).
        let requisicaoComLimite = requisicao
                { HTTP.responseTimeout = HTTP.responseTimeoutMicro 5000000 }
        -- 3) "try" captura a exceção se a rede falhar, em vez de derrubar
        --    a requisição com erro 500.
        resultado <- liftIO (try (HTTP.httpLbs requisicaoComLimite manager))
            :: Handler (Either HTTP.HttpException (HTTP.Response LByteString))
        return $ case resultado of
            Left _ -> Left ServicoIndisponivel
            Right resposta
                | HTTP.responseStatus resposta /= status200 ->
                    Left ServicoIndisponivel
                | otherwise ->
                    -- 4) Se o JSON não tem os campos do endereço, é porque
                    --    o ViaCEP respondeu {"erro": "true"}.
                    case decode (HTTP.responseBody resposta) of
                        Nothing       -> Left CepNaoEncontrado
                        Just endereco -> Right endereco
  where
    cepLimpo = limparCep cep

-- | Transforma cada erro em uma resposta HTTP com status adequado.
-- Usada pelos handlers de CEP e de endereço do aluno.
responderErroViaCep :: ErroViaCep -> Handler a
responderErroViaCep falha = sendStatusJSON status (object ["erro" .= mensagem])
  where
    (status, mensagem) = case falha of
        CepInvalido ->
            (status400, "CEP inválido: informe 8 dígitos" :: Text)
        CepNaoEncontrado ->
            (status404, "CEP não encontrado")
        ServicoIndisponivel ->
            (status502, "Não foi possível consultar o ViaCEP. Tente novamente mais tarde.")

-- | Monta a URL: https://viacep.com.br/ws/01001000/json/
urlViaCep :: Text -> String
urlViaCep cep = "https://viacep.com.br/ws/" ++ T.unpack cep ++ "/json/"
