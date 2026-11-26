{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Comunicação com a API externa ViaCEP (https://viacep.com.br).
-- Recebe um CEP, faz uma requisição HTTP para o ViaCEP e converte o JSON
-- da resposta em um tipo Haskell que o resto do sistema pode usar.
module Service.ViaCep
    ( EnderecoViaCep (..)
    , buscarCep
    , limparCep
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

-- | Como ler o JSON do ViaCEP. Exemplo de resposta:
-- { "cep": "01001-000", "logradouro": "Praça da Sé", "bairro": "Sé",
--   "localidade": "São Paulo", "uf": "SP", ... }
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
-- Devolve Nothing quando o ViaCEP não encontra o CEP: nesse caso ele
-- responde {"erro": "true"}, que não tem os campos acima.
buscarCep :: Text -> Handler (Maybe EnderecoViaCep)
buscarCep cep = do
    manager    <- getsYesod appHttpManager
    requisicao <- liftIO $ HTTP.parseRequest (urlViaCep cep)
    resposta   <- HTTP.httpLbs requisicao manager
    return $ decode (HTTP.responseBody resposta)

-- | Monta a URL: https://viacep.com.br/ws/01001000/json/
urlViaCep :: Text -> String
urlViaCep cep = "https://viacep.com.br/ws/" ++ T.unpack (limparCep cep) ++ "/json/"
