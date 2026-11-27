{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Cep
    ( getCepR
    ) where

import Import
import Service.ViaCep

-- | GET /cep/#Text — consulta um CEP na API externa ViaCEP.
-- Exemplo: GET /cep/01001000
-- Erros: 400 (CEP inválido), 404 (não encontrado), 502 (ViaCEP fora do ar).
getCepR :: Text -> Handler Value
getCepR cep = do
    resultado <- buscarCep cep
    case resultado of
        Left falha     -> responderErroViaCep falha
        Right endereco -> returnJson endereco
