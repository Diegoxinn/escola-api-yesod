{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Cep where

import Import
import Service.ViaCep

-- | GET /cep/#Text — consulta um CEP na API externa ViaCEP.
-- Exemplo: GET /cep/01001000
getCepR :: Text -> Handler Value
getCepR cep = do
    resultado <- buscarCep cep
    case resultado of
        Nothing       -> sendStatusJSON status404
                            (object ["erro" .= ("CEP não encontrado" :: Text)])
        Just endereco -> returnJson endereco
