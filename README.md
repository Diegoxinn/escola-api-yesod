# Escola API

```
+--------------------------------------------+
|   ESCOLA API  ·  Haskell + Yesod + SQLite  |
+--------------------------------------------+
```

Projeto final da disciplina de **Haskell (P2)**: uma API REST para gestão escolar,
com cursos, professores, disciplinas, turmas, alunos, matrículas, notas e endereços
(preenchidos pela API externa [ViaCEP](https://viacep.com.br)). Testada no Postman!

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
           │          │
           │          └──< Nota
           └──── Endereco (1:1, dados do ViaCEP)
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
| Endereco   | alunoId (único), cep, logradouro, numero, complemento?, bairro, cidade, uf |


Exemplo de corpo:

```json
{ "nome": "Sistemas para internet", "descricao": "Tecnologo", "cargaHoraria": 3200 }
```

## Endpoints

Todas as respostas são em JSON. Nos `POST`/`PUT`, envie o cabeçalho
`Content-Type: application/json`.

| Recurso | Método | Rota | Descrição |
|---|---|---|---|
| Status | GET | `/` | Verifica se a API está no ar |
| Cursos | GET, POST | `/cursos` | Lista / cadastra cursos |
| | GET, PUT, DELETE | `/cursos/{id}` | Busca / altera / remove um curso |
| Professores | GET, POST | `/professores` | Lista / cadastra professores |
| | GET, PUT, DELETE | `/professores/{id}` | Busca / altera / remove um professor |
| Disciplinas | GET, POST | `/disciplinas` | Lista / cadastra disciplinas |
| | GET, PUT, DELETE | `/disciplinas/{id}` | Busca / altera / remove uma disciplina |
| Turmas | GET, POST | `/turmas` | Lista / cadastra turmas |
| | GET, PUT, DELETE | `/turmas/{id}` | Busca / altera / remove uma turma |
| | GET | `/turmas/{id}/matriculas` | Matrículas da turma |
| Alunos | GET, POST | `/alunos` | Lista (filtro `?cursoId=`) / cadastra alunos |
| | GET, PUT, DELETE | `/alunos/{id}` | Busca / altera / remove um aluno |
| | GET | `/alunos/{id}/matriculas` | Matrículas do aluno |
| | GET | `/alunos/{id}/boletim` | Boletim do aluno |
| Matrículas | GET, POST | `/matriculas` | Lista / cadastra matrículas |
| | GET, PUT, DELETE | `/matriculas/{id}` | Busca / altera / remove uma matrícula |
| | GET | `/matriculas/{id}/notas` | Notas da matrícula |
| Notas | GET, POST | `/notas` | Lista / lança notas |
| | GET, PUT, DELETE | `/notas/{id}` | Busca / altera / remove uma nota |
| CEP | GET | `/cep/{cep}` | Consulta um CEP no ViaCEP |
| Endereço | GET, POST, PUT, DELETE | `/alunos/{id}/endereco` | Endereço do aluno (dados do ViaCEP) |

Códigos de status usados: `200` OK, `201` criado, `204` removido, `400` dados
inválidos, `404` não encontrado, `409` conflito (registro duplicado ou com
vínculos) e `502` falha no serviço externo.

## Integração com API externa (ViaCEP)

O cadastro de endereço do aluno usa o [ViaCEP](https://viacep.com.br). O cliente
envia só o CEP, o número e o complemento; a API consulta o ViaCEP e preenche
logradouro, bairro, cidade e UF antes de salvar no banco.

```http
POST /alunos/1/endereco
Content-Type: application/json

{ "cep": "01001000", "numero": "100", "complemento": "Apto 12" }
```

Resposta `201`:

```json
{
  "id": 1, "alunoId": 1, "cep": "01001-000", "logradouro": "Praça da Sé",
  "numero": "100", "complemento": "Apto 12", "bairro": "Sé",
  "cidade": "São Paulo", "uf": "SP"
}
```

| Situação | Status | Resposta |
|---|---|---|
| CEP sem 8 dígitos | 400 | `{"erro": "CEP inválido: informe 8 dígitos"}` |
| CEP não existe | 404 | `{"erro": "CEP não encontrado"}` |
| ViaCEP fora do ar, sem internet ou demora acima de 5 s | 502 | `{"erro": "Não foi possível consultar o ViaCEP. ..."}` |
| Aluno já tem endereço (POST) | 409 | `{"erro": "Este aluno já possui endereço. Use PUT para alterar."}` |

Código: `src/Service/ViaCep.hs` (comunicação com o ViaCEP), `src/Handler/Cep.hs`
e `src/Handler/Endereco.hs`.

## Testes no Postman

A coleção `postman/Escola-API.postman_collection.json` tem todas as requisições
na ordem de demonstração. No Postman: **Import**, arraste o arquivo e use
**Run collection** com a API rodando. Comece pela pasta `0. Status`: ela gera um
sufixo que evita e-mails e códigos repetidos, e os ids criados são guardados
automaticamente nas variáveis da coleção.

## Contribuições

| Integrante | Branch | Contribuição |
|---|---|---|
| Gabrielle Almeida | `feature/estrutura-inicial` | Configuração do Yesod e SQLite, modelagem das tabelas, CRUD de Curso, README inicial |
| Kauê de Oliveira | `feature/crud-modulo-1` | CRUD de Professor, Disciplina e Turma |
| Diego Ferreira | `feature/crud-modulo-2` | CRUD de Aluno, Matrícula e Nota, boletim |
| Wellington Junior | `feature/integracao-api` | Integração com o ViaCEP, endereço do aluno, tratamento de erros, coleção do Postman e documentação |
