{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module ModelSpec (spec) where

import TestImport
import Data.Time (fromGregorian)

spec :: Spec
spec = withApp $ do
    describe "Banco de dados" $ do
        it "a migração cria as 7 tabelas" $ do
            tabelas <- runDB getTables
            forM_ ["curso", "professor", "disciplina", "turma", "aluno", "matricula", "nota"] $ \t ->
                assertEq ("tabela " ++ unpack t) True (t `elem` tabelas)

        it "relaciona curso, disciplina, turma, aluno, matrícula e nota" $ do
            notas <- runDB $ do
                cursoId <- insert $ Curso "Ciência da Computação" Nothing 3200
                profId  <- insert $ Professor "Ana Souza" "ana@escola.com" "Programação Funcional"
                discId  <- insert $ Disciplina "Haskell" 60 cursoId
                turmaId <- insert $ Turma "HASK-2026-1" "2026.1" discId profId
                alunoId <- insert $ Aluno "João Silva" "joao@escola.com" "RA001" (fromGregorian 2004 5 10) cursoId
                matId   <- insert $ Matricula alunoId turmaId (fromGregorian 2026 2 1)
                _       <- insert $ Nota matId "P1" 8.5
                selectList [NotaMatriculaId ==. matId] []
            assertEq "uma nota cadastrada" 1 (length notas)