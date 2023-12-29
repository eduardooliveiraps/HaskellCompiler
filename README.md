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

Na Parte 1, para além da definição das instruções da máquina já fornecida pelos professores:

```haskell
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst] 
```

### Parte 2 
