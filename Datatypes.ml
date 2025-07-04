(* operações binárias *)
type bop =
  | Sum | Sub | Mul | Div   (* operações aritméticas *)
  | Eq  | Neq | Lt | Gt   (* operações relacionais  *)
  | And | Or   (* operações lógicas *)

(* tipos *)
type tipo =
  | TyInt
  | TyBool
  | TyRef of tipo
  | TyUnit

(* expressões *)
type expr =
  | Num of int
  | Bool of bool
  | Loc of int   (* location *)
  | Id of string   (* variável *)
  | If of expr * expr * expr
  | Binop of bop * expr * expr   (* operações binárias *)
  | Wh of expr * expr   (* while *)
  | Asg of expr * expr   (* assignment *)
  | Let of string * tipo * expr * expr
  | New of expr
  | Deref of expr
  | Unit
  | Seq of expr * expr
  | Read
  | Print of expr
  | For of expr * expr * expr * expr (* como em C: for (e1; e2; e3) { e4 } *)

type environment = (string * tipo) list

type memory = {
  mutable num_locations: int;
  mutable locations: expr array;
}

(* Procura o tipo associado á variável x no ambiente env *)
let rec lookup (env : environment) (x : string) : tipo option =
  match env with
  | [] -> None
  | (y, t) :: rest -> if x = y then Some t else lookup rest x

(* Adiciona uma nova extensão x:t no env *)
let extend (env : environment) (x : string) (t : tipo) : environment =
  (x, t) :: env

let rec typeInfer (env: environment) (e:expr) : tipo option =
  match e with
  (* T-INT *)
  | Num n -> Some TyInt

  (* T-BOOL *)
  | Bool b -> Some TyBool

  (* T-OPx *)
  | Binop (op, e1, e2) -> (match op with
      | Sum | Sub | Mul | Div -> (match typeInfer env e1, typeInfer env e2 with
          | Some TyInt, Some TyInt -> Some TyInt
          | _ -> None)
      | Eq | Neq | Lt | Gt -> (match typeInfer env e1, typeInfer env e2 with
          | Some TyInt, Some TyInt -> Some TyBool
          | _ -> None)
      | And | Or -> (match typeInfer env e1, typeInfer env e2 with 
          | Some TyBool, Some TyBool -> Some TyBool 
          | _ -> None))

  (* T-IF *)
  | If (e1, e2, e3) -> (match typeInfer env e1 with
      | Some TyBool -> (match typeInfer env e2, typeInfer env e3 with
          | Some t2, Some t3 -> if t2=t3
              then Some t2 else None
          | _ -> None)
      | _  -> None)

  (* T-VAR *)
  | Id x -> lookup env x
  | Loc _ -> None

  (* T-LET *)
  | Let (x, t, e1, e2) ->
      (match typeInfer env e1 with
       | Some t1 when t1 = t ->
           let env' = extend env x t in
           typeInfer env' e2
       | _ -> None)

  (* T-ATR *) 
  | Asg (e1, e2) -> (match typeInfer env e1, typeInfer env e2 with
      | Some TyRef t1, Some t2 -> if t1=t2
          then Some TyUnit else None
      | _ -> None)

  (* T-DEREF *)
  | Deref e -> (match typeInfer env e with
      | Some TyRef t -> Some t
      | _ -> None)

  (* T-NEW *)
  | New e -> (match typeInfer env e with
      | Some t -> Some (TyRef t)
      | _ -> None)

  (* T-UNIT *)
  | Unit -> Some TyUnit

  (* T-WHILE *)
  | Wh (e1, e2) -> (match typeInfer env e1, typeInfer env e2 with
      | Some TyBool, Some TyUnit -> Some TyUnit
      | _ -> None)

  (* T-SEQ *)
  | Seq (e1, e2) -> (match typeInfer env e1, typeInfer env e2 with
      | Some TyUnit, Some t -> Some t
      | _ -> None)

  (* T-READ *)
  | Read -> Some TyInt

  (* T-PRINT *)
  | Print e -> (match typeInfer env e with
      | Some TyInt -> Some TyUnit
      | _ -> None)

  (* T-FOR 
    
     Esta regra define o tipo da construção `For(e1, e2, e3, e4)`.

     A expressão `For(e1, e2, e3, e4)` representa um laço imperativo com os seguintes componentes:
       - e1: expressão de inicialização (executada uma vez antes do loop)
       - e2: condição booleana (avaliada a cada iteração para decidir se continua)
       - e3: corpo do laço (executado a cada iteração se a condição for verdadeira)
       - e4: incremento (executado ao final de cada iteração)

     Regras de tipagem:
       - e1 deve ter tipo TyUnit: a inicialização pode ser qualquer expressão que produza efeito (como uma atribuição)
       - e2 deve ter tipo TyBool: a condição de parada do laço precisa ser booleana
       - e3 deve ter tipo TyUnit: o corpo do laço é imperativo (efeitos colaterais) e não retorna valor útil
       - e4 deve ter tipo TyUnit: o incremento também é imperativo (como uma atribuição)

     Se todas essas condições forem satisfeitas, o tipo do `For` será TyUnit.

*)

  | For (e1, e2, e3, e4) ->
      (match typeInfer env e1, typeInfer env e2, typeInfer env e3, typeInfer env e4 with
       | Some TyUnit, Some TyBool, Some TyUnit, Some TyUnit -> Some TyUnit
       | _ -> None)


let is_value (e:expr) : bool =
  match e with
  | Num e -> true
  | Bool e -> true
  | Unit -> true
  | Loc e -> true
  | _ -> false


(* Função para substituição {v/x} e *)
let rec subst (x:string) (v:expr) (e:expr) : expr =
  match e with
  | Num _ | Bool _ | Unit | Loc _ -> e
  | Id y -> if x = y then v else Id y
  | If (e1, e2, e3) -> If (subst x v e1, subst x v e2, subst x v e3)
  | Binop (op, e1, e2) -> Binop (op, subst x v e1, subst x v e2)
  | Wh (e1, e2) -> Wh (subst x v e1, subst x v e2)
  | Asg (e1, e2) -> Asg (subst x v e1, subst x v e2)
  | Let (y, t, e1, e2) ->
      if x = y then Let (y, t, subst x v e1, e2)  (* não substitui no corpo para não modificar variavel ligada *)
      else Let (y, t, subst x v e1, subst x v e2)
  | New e1 -> New (subst x v e1)
  | Deref e1 -> Deref (subst x v e1)
  | Seq (e1, e2) -> Seq (subst x v e1, subst x v e2)
  | Read -> Read
  | Print e1 -> Print (subst x v e1)
  | For (e1, e2, e3, e4) -> For (subst x v e1, subst x v e2, subst x v e3, subst x v e4)


let rec step (e:expr) (mem: memory) (inp:int list) (out:int list) :
  (expr * memory * int list * int list) option =
  match e with
  (* OPx *)

  (* Operações com argumentos inteiros *)
  | Binop (op, Num e1, Num e2) -> (match op with
      | Sum -> Some (Num (e1 + e2), mem, inp, out)
      | Sub -> Some (Num (e1 - e2), mem, inp, out)
      | Mul -> Some (Num (e1 * e2), mem, inp, out)
      | Div -> Some (Num (e1 / e2), mem, inp, out)
      | Eq -> Some (Bool (e1 = e2), mem, inp, out)
      | Neq -> Some (Bool (e1 <> e2), mem, inp, out)
      | Lt -> Some (Bool (e1 < e2), mem, inp, out)
      | Gt -> Some (Bool (e1 > e2), mem, inp, out)
      | _ -> None)

  (* Operações com argumentos booleanos *)
  | Binop (op, Bool e1, Bool e2) -> (match op with
      | And -> Some (Bool (e1 && e2), mem, inp, out)
      | Or -> Some (Bool (e1 || e2), mem, inp, out)
      | _ -> None)

  (* OP2 *)
  | Binop (op, e1, e2) when is_value e1 -> (match step e2 mem inp out with
      | Some (e2', mem', inp', out') -> Some (Binop (op, e1, e2'), mem', inp', out')
      | _ -> None)

  (* OP1 *)
  | Binop (op, e1, e2) -> (match step e1 mem inp out with
      | Some (e1', mem', inp', out') -> Some (Binop (op, e1', e2), mem', inp', out')
      | _ -> None)

  (* IF1 *)
  | If (Bool true, e2, e3) -> Some (e2, mem, inp, out)

  (* IF2 *)
  | If (Bool false, e2, e3) -> Some (e3, mem, inp, out)

  (* IF3 *)
  | If (e1, e2, e3) -> (match step e1 mem inp out with
      | Some (e1', mem', inp', out') -> Some (If (e1', e2, e3), mem', inp', out')
      | _ -> None)

  (* E-LET2 *)
  | Let (x, t, e1, e2) when is_value e1 -> Some (subst x e1 e2, mem, inp, out)

  (* E-LET1 *)
  | Let (x, t, e1, e2) -> (match step e1 mem inp out with
      | Some (e1', mem', inp', out') -> Some (Let (x, t, e1', e2), mem', inp', out')
      | _ -> None)

  (* ATR1 *)
  | Asg (Loc e1, e2) when e1 < mem.num_locations && is_value e2 ->
      mem.locations.(e1) <- e2;
      Some (Unit, mem, inp, out)

  (* ATR2 *)
  | Asg (Loc e1, e2) -> (match step e2 mem inp out with
      | Some (e2', mem', inp', out') -> Some (Asg (Loc e1, e2'), mem', inp', out')
      | _ -> None)

  (* ATR *)
  | Asg (e1, e2) -> (match step e1 mem inp out with
      | Some (e1', mem', inp', out') -> Some (Asg (e1', e2), mem', inp', out')
      | _ -> None)

  (* DEREF1 *)
  | Deref (Loc e1) when e1 < mem.num_locations -> Some (mem.locations.(e1), mem, inp, out)

  (* DEREF *)
  | Deref e1 -> (match step e1 mem inp out with
      | Some (e1', mem', inp', out') -> Some (Deref e1', mem', inp', out')
      | _ -> None)

  (* NEW1 *)
  | New e1 when is_value e1 ->
      let location = mem.num_locations in
      mem.locations.(location) <- e1;
      mem.num_locations <- mem.num_locations + 1;
      Some (Loc location, mem, inp, out);

  (* NEW *)
  | New e1 -> (match step e1 mem inp out with
      | Some (e1', mem', inp', out') -> Some (New e1', mem', inp', out')
      | _ -> None)

  (* SEQ1 *)
  | Seq (Unit, e2) -> Some (e2, mem, inp, out)

  (* SEQ *)
  | Seq (e1, e2) -> (match step e1 mem inp out with
      | Some (e1', mem', inp', out') -> Some (Seq (e1',e2), mem', inp', out')
      | _ -> None)

  (* E-WHILE *)
  | Wh (e1, e2) -> Some (If (e1, Seq (e2, Wh (e1, e2)), Unit), mem, inp, out)

  (* PRINT-N *)
  | Print (Num e1) -> Some (Unit, mem, inp, out @ [e1])

  (* PRINT *)
  | Print e1 -> (match step e1 mem inp out with
      | Some (e1', mem', inp', out') -> Some (Print e1', mem', inp', out')
      | _ -> None)

  (* READ *)
  | Read -> (match inp with
      | h :: t -> Some (Num h, mem, t, out)
      | _ -> None)

  (* E-FOR *)
  | For (e1, e2, e3, e4) -> Some (Seq (e1, Wh (e2, Seq (e4, e3))), mem, inp, out)

  | _ -> None


let rec steps (e:expr) (mem: memory) (inp:int list) (out:int list) :
  (expr * memory * int list * int list) =
  match step e mem inp out with
  | None -> (e, mem, inp, out)
  | Some (e', mem', inp', out') -> steps e' mem' inp' out'


(* casos de teste *)

(*

let  x: int     =  read() in
let  z: ref int = new x in
let  y: ref int = new 1 in

(while (!z > 0) (
     y :=  !y * !z;
     z :=  !z - 1);
   print (! y))

*)

let cndwhi = Binop(Gt, Deref (Id "z"),Num 0)
let asgny = Asg(Id "y", Binop(Mul, Deref (Id "y"),Deref(Id "z")))
let asgnz = Asg(Id "z", Binop(Sub, Deref (Id "z"),Num 1))
let bdwhi = Seq(asgny, asgnz) 
let whi = Wh(cndwhi, bdwhi)
let prt = Print(Deref (Id "y"))
let seq = Seq(whi, prt)

let fat = Let("x", TyInt, Read,
              Let("z", TyRef TyInt, New (Id "x"),
                  Let("y", TyRef TyInt, New (Num 1),
                      seq)))

let for_expr =
  Let("i", TyRef TyInt, New (Num 0),
      For (
        Asg (Id "i", Num 0),                                  (* inicialização: i = 0 *)
        Binop (Lt, Deref (Id "i"), Num 3),                    (* condição: i < 3 *)
        Asg (Id "i", Binop (Sum, Deref (Id "i"), Num 1)),     (* incremento: i = i + 1 *)
        Print (Deref (Id "i"))))                              (* corpo: print(i) *)

let ex1 =
  Let("x",TyRef(TyInt), New(Num(3)),
      Seq( 
        Asg(Id("x") , Binop(Sum,Read,Num(1))),
        Print(Deref(Id("x")))
      )
     )
;;

let ex2 =
  Let("x",TyBool, Bool(true),
      Seq(
        Let("x",TyInt, Num(3),
            Print(Binop(Sum,Id("x"),Num(1)))
           )
        ,
        Id("x")
      )
     )
;;

let ex3 = If(Binop(Lt,Num(3),Num(5)),
             Bool(true),
             Unit)
;;

let ex4 =
  Let("x",TyInt,Num(4),
      Let("y",TyRef TyInt,New(Num(0)),
          Let("a",TyRef TyInt,New(Num(0)),
              Wh(Binop(Lt,Deref(Id("y")),Id("x")),
                 Seq(
                   Asg(Id("y"),Binop(Sum,Deref(Id("y")),Num(1)))
                   ,
                   Asg(Id("a"),Binop(Sum,Deref(Id("a")),Deref(Id("y"))))
                 )
                )
             )
         )
     )
;;

let ex5 =
  Let ("y", TyRef TyBool, New(Bool(true)),
       If(
         Binop(Lt,Deref(New(Num(5))), Num(2)),
         New(Bool(false)),
         Id("y")
       )
      )
;;

let ex6 =
  Let("x",TyRef TyInt, New(Num(0)),
      Let("a",TyRef TyInt,New(Num(1)),
          Seq(
            Asg(Id("x"), Read)
            ,
            Seq(
              Wh(
                Binop(Neq, Deref(Id("x")), Num(0) )
                ,
                Seq(
                  Asg(Id("a"),Binop(Mul,Deref(Id("a")),Deref(Id("x"))))
                  ,
                  Asg(Id("x"),Binop(Sub,Deref(Id("x")),Num(1)))
                )
              )
              ,
              Print(Deref(Id("a")))
            )
          )
         )
     )
;;
