# <p align="center">Interpretador Linguagem L2 🐪</p>

Trabalho desenvolvido na disciplina de Semântica Formal (INF05516) pelos alunos João Speranza Pastorello, Gabriel Difforeni Leal e Matheus Manica da Silva.

O trabalho consiste em implementar em **OCaml** um interpretador para a linguagem L2 [desta especificação](especificacao.pdf).

Como variação, foi implementada a expressão _for_ para repetições. Essa segue estrutura inspirada em C.

## Como usar

Recomenda-se seguir [este tutorial](https://cs3110.github.io/textbook/chapters/preface/install.html) para instalar OCaml no seu Sistema Operacional.

### Usando utop

`utop` é uma interface de linha de comando para OCaml, similar ao interpretador `python`.

```shell
utop
```

Para sair do `utop`, execute:

```ocaml
#quit;;
```

Para importar o código do interpretador, execute:

```ocaml
#use "Datatypes.ml";;
```

#### Sistema de tipos

Para utilizar o sistema de tipos, é preciso primeiro definir um ambiente.

Para criar um ambiente vazio com 10 posições (que cresce automaticamente conforme a necessidade), execute:

```ocaml
let env = Hashtbl.create 10;;
```

A inferência de tipos é realizada por meio da função typeInfer:

```ocaml
typeInfer env <expressão>;;
```

Por exemplo, para a expressão que representa (2 > 1), o tipo é booleano (TyBool):

```ocaml
typeInfer env (Binop(Gt, Num 2, Num 1));;
- : tipo option = Some TyBool
```


#### Interpretador small-step

Para utilizar o interpretador small-step, é preciso primeiro definir a memória e as listas de entrada e saída.

Para criar uma memória com 10 posições (tamanho fixo) inicializadas com o valor 0, execute:

```ocaml
let mem = {num_locations=0; locations=Array.make 10 0};;
```

As listas de entrada e saída podem ser definidas naturalmente com a [sintaxe de OCaml para listas](https://cs3110.github.io/textbook/chapters/data/lists.html#building-lists).

O passo da interpretação é realizado por meio da função step:

```ocaml
step <expressão> mem <entrada> <saída>;;
```

A função steps realiza múltiplos passos, avaliando a expressão até a sua forma normal:

```ocaml
steps <expressão> mem <entrada> <saída>;;
```

Por exemplo, para o programa que implementa o fatorial do primeiro valor da lista de entrada, a expressão avalia para Unit e o resultado é colocado na lista de saída:

```ocaml
steps fat mem [5] [];;
- : expr * memory * int list * int list =
(Unit, {num_locations = 2; locations = [|0; 120; 0; 0; 0; 0; 0; 0; 0; 0|]},
 [], [120])
```
