# PFL_TP2_T10_G10

| Nome                         | UP            | Contribuição |
| ------------                 | ------------  |------------  |
| Bruno Miguel de Siuéia Duvane| [up202109244] |50%           |
| Eduardo Martins Oliveira     | [up202108690] |50%           |

## Descrição do Projeto

O tema deste projeto gira em torno da criação de uma máquina de baixo-nível com configurações da forma `(c, e, s)` onde `c` é uma lista de instruções (ou código) a ser executada, `e` é um stack de avaliação, e `s` é o armazenamento. A máquina deve ser capaz de interpretar e executar instruções.

## Estratégia Utilizada na Implementação

Este projeto divide-se em duas partes. Em ambas, começámos pela definição dos tipos de dados necessários para representar as instruções, a stack, o estado e os vários tipos de expressões. Só depois, passámos à implementação das funções necessárias, que estão descritas no guião fornecido do projeto.

### Parte 1

Na Parte 1, para além da definição das instruções da máquina já fornecida pelos professores (`data Inst`), criámos dois tipos de dados (`Stack` e `State`). Os elementos da Stack podem ser do tipo Integer (número positivo ou negativo) ou String ("tt" ou "ff", verdadeiro ou falso, respetivamente). Os elementos do State são pares da forma (variável, valor), onde a variável é uma String e o valor, à semelhança dos elementos da Stack, é um Integer ou uma String.

```haskell
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst] 
```

```haskell
type Stack = [Either Integer String] -- A type that represents the machine’s stack

type State = [(String, Either Integer String)] -- A type that represents the machine’s state
```

Funções implementadas na Parte 1:

- `createEmptyStack :: Stack`: Função que retorna uma pilha da máquina vazia
- `createEmptyState :: State`: Função que retorna um estado da máquina vazio
- `stack2Str :: Stack -> String`: Função que converte uma Stack numa string. A função utiliza a função map para aplicar a transformação necessária a cada elemento da stack, juntamente com a função intercalate "," que concatena os elementos, separando-os por vírgula. 
- `state2Str :: State -> String`: Função que converte um State numa string. A função utiliza a função map para aplicar a transformação necessária a cada elemento da stack, juntamente com a função intercalate "," que concatena os elementos, separando-os por vírgula. Adicionalmente, utiliza a função sort para ordenar os elementos por ordem alfabética, onde a chave de ordenação é a string que corresponde ao nome da variável.
- `run :: (Code, Stack, State) -> (Code, Stack, State)`: Função que executa as instruções (Code), atualizando a Stack e o State de acordo. Na implementação desta função utilizámos uma lógica por casos, sendo cada caso um tipo de instrução diferente. 

### Parte 2 

Na Parte 2, definimos três tipos `Aexp` (expressões aritméticas), `Bexp` (expressões booleanas) e `Stm`(declarações). 

```haskell
data Aexp = VarAexp String | IntAexp Integer | AddAexp Aexp Aexp | SubAexp Aexp Aexp | MultAexp Aexp Aexp
    deriving (Show)

data Bexp = BoolBexp Bool | EqBexp Aexp Aexp | LeBexp Aexp Aexp | NotBexp Bexp | AndBexp Bexp Bexp
    deriving (Show)

data Stm = AssignStm String Aexp | SeqStm Stm Stm | IfStm Bexp Stm Stm | WhileStm Bexp Stm | NoopStm
  deriving (Show)
type Program = [Stm] 
```

Nota: Na definição das declarações: `AssignStm String Aexp` corresponde à atribuição de um valor (Aexp) a uma variável (String); `SeqStm Stm Stm` corresponde a uma sequência de instruções; `IfStm Bexp Stm Stm` corresponde a uma declaração if (branch da Parte 1); `WhileStm Bexp Stm` corresponde à declaração de um ciclo while (loop da Parte 1); NoopStm corresponde a uma instrução que não faz nada.

Funções implementadas na Parte 2:
