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
  | Var of string
  | Id of int   (* endereço de memória *)
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

type memory = {
    mutable num_locations: int;
    mutable locations: int array;
}


let rec typeInfer (e:expr) : tipo option =
  match e with
  (* T-INT *)
  | Num n -> Some TyInt

  (* T-BOOL *)
  | Bool b -> Some TyBool

  (* T-OPx *)
  | Binop (op, e1, e2) -> (match op with
      | Sum | Sub | Mul | Div -> (match typeInfer e1, typeInfer e2 with
          | Some TyInt, Some TyInt -> Some TyInt
          | _ -> None)
      | Eq | Neq | Lt | Gt -> (match typeInfer e1, typeInfer e2 with
          | Some TyInt, Some TyInt -> Some TyBool
          | _ -> None)
      | And | Or -> (match typeInfer e1, typeInfer e2 with 
          | Some TyBool, Some TyBool -> Some TyBool 
          | _ -> None))

  (* T-IF *)
  | If (e1, e2, e3) -> (match typeInfer e1 with
      | Some TyBool -> (match typeInfer e2, typeInfer e3 with
          | Some t2, Some t3 -> if t2=t3
              then Some t2 else None
          | _ -> None)
      | _  -> None)

  (* TODO: T-VAR *)
  | Id e' -> None
  | Var e' -> None

  (* TODO: T-LET *)
  | Let (x, t, e1, e2) -> (match t, typeInfer e1, typeInfer e2 with
      | t, Some t1, Some t2 -> if t1=t
          then Some t2 else None
      | _ -> None)

  (* T-ATR *)
  | Asg (e1, e2) -> (match typeInfer e1, typeInfer e2 with
      | Some TyRef t1, Some t2 -> if t1=t2
          then Some TyUnit else None
      | _ -> None)

  (* T-DEREF *)
  | Deref e -> (match typeInfer e with
      | Some TyRef t -> Some t
      | _ -> None)

  (* T-NEW *)
  | New e -> (match typeInfer e with
      | Some t -> Some t
      | _ -> None)

  (* T-UNIT *)
  | Unit -> Some TyUnit

  (* T-WHILE *)
  | Wh (e1, e2) -> (match typeInfer e1, typeInfer e2 with
      | Some TyBool, Some TyUnit -> Some TyUnit
      | _ -> None)

  (* T-SEQ *)
  | Seq (e1, e2) -> (match typeInfer e1, typeInfer e2 with
      | Some TyUnit, Some t -> Some t
      | _ -> None)

  (* T-READ *)
  | Read -> Some TyInt

  (* T-PRINT *)
  | Print e -> (match typeInfer e with
      | Some TyInt -> Some TyUnit
      | _ -> None)


let is_value (e:expr) : bool =
  match e with
  | Num e -> true
  | Bool e -> true
  | Id e -> true
  | Unit -> true
  | _ -> false


(* Função para substituição {v/x} e *)
let rec subst (x:string) (v:expr) (e:expr) : expr =
  match e with
  | Num _ | Bool _ | Id _ | Unit -> e
  | Var y -> if x = y then v else Var y
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
  | Asg (Id e1, Num e2) when e1 < mem.num_locations ->
      mem.locations.(e1) <- e2;
      Some (Unit, mem, inp, out)

  (* ATR2 *)
  | Asg (Id e1, e2) -> (match step e2 mem inp out with
      | Some (e2', mem', inp', out') -> Some (Asg (Id e1, e2'), mem', inp', out')
      | _ -> None)

  (* ATR *)
  | Asg (e1, e2) -> (match step e1 mem inp out with
      | Some (e1', mem', inp', out') -> Some (Asg (e1', e2), mem', inp', out')
      | _ -> None)

  (* DEREF1 *)
  | Deref (Id e1) when e1 < mem.num_locations -> Some (Num mem.locations.(e1), mem, inp, out)

  (* DEREF *)
  | Deref e1 -> (match step e1 mem inp out with
      | Some (e1', mem', inp', out') -> Some (Deref e1', mem', inp', out')
      | _ -> None)

  (* NEW1 *)
  | New (Num e1) ->
      let location = mem.num_locations in
      mem.locations.(location) <- e1;
      mem.num_locations <- mem.num_locations + 1;
      Some (Id location, mem, inp, out);

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
  | Read -> Some (
      (match inp with
        | h :: t -> Num h
        | _ -> Unit), mem, inp, out)

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

let cndwhi = Binop(Gt, Deref (Var "z"),Num 0)
let asgny = Asg(Var "y", Binop(Mul, Deref (Var "y"),Deref(Var "z")))
let asgnz = Asg(Var "z", Binop(Sub, Deref (Var "z"),Num 1))
let bdwhi = Seq(asgny, asgnz) 
let whi = Wh(cndwhi, bdwhi)
let prt = Print(Deref (Var "y"))
let seq = Seq(whi, prt)

let fat = Let("x", TyInt, Read,
              Let("z", TyRef TyInt, New (Var "x"),
                  Let("y", TyRef TyInt, New (Num 1),
                      seq)))
