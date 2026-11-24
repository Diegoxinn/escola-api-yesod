{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import

-- | GET / — verifica se a API está no ar e responde em JSON.
getHomeR :: Handler Value
getHomeR = return $ object
    [ "status"   .= ("ok" :: Text)
    , "mensagem" .= ("API REST em Yesod + SQLite" :: Text)
    ]