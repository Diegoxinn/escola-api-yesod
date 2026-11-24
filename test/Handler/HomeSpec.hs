{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "GET /" $ do
        it "responde 200 com o status da API em JSON" $ do
            get HomeR
            statusIs 200
            bodyContains "\"status\":\"ok\""