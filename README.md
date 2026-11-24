# Escola API

```
+--------------------------------------------+
|   ESCOLA API  ·  Haskell + Yesod + SQLite  |
+--------------------------------------------+
```

Projeto final da disciplina de **Haskell (P2)**: uma API REST para gestão escolar,
com cursos, professores, disciplinas, turmas, alunos, matrículas e notas. Testada no Postman!

## Equipe

- Kaue de Oliveira
- Diego Ferreira
- Gabrielle Almeida
- Wellington Junior

## Como rodar

Requisito: [Stack](https://docs.haskellstack.org/).

```bash
stack install yesod-bin --install-ghc   # só na primeira vez
stack build
stack exec -- yesod devel
```

A API sobe em `http://localhost:3000`. O banco `meu-projeto.sqlite3` e as tabelas
são criados automaticamente na primeira execução.

Testes:

```bash
stack test --flag meu-projeto:library-only --flag meu-projeto:dev
```

## Modelo de dados

```
Curso ──< Disciplina ──< Turma >── Professor
  │                        │
  └────< Aluno ──< Matricula >┘
                      │
                      └──< Nota
```

| Tabela     | Campos                                              |
|------------|-----------------------------------------------------|
| Curso      | nome (único), descricao?, cargaHoraria              |
| Professor  | nome, email (único), especialidade                  |
| Disciplina | nome, cargaHoraria, cursoId                         |
| Turma      | codigo (único), semestre, disciplinaId, professorId |
| Aluno      | nome, email (único), ra (único), dataNascimento, cursoId |
| Matricula  | alunoId, turmaId, dataMatricula (par aluno/turma único) |
| Nota       | matriculaId, descricao, valor                       |


Exemplo de corpo:

```json
{ "nome": "Sistemas para internet", "descricao": "Tecnologo", "cargaHoraria": 3200 }
```
