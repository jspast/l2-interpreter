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
  | Id of string   (* endereço de memória *)
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
    | Eq | Neq | Lt | Gt | And | Or -> (match typeInfer e1, typeInfer e2 with
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

  (* T-LET *)
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
  | _ -> false

let rec step (e:expr) (mem:(string, expr) Hashtbl.t) (inp:int list) (out:int list) :
                (expr *     (string, expr) Hashtbl.t *     int list *     int list) option =
  match e with
  (* OPx *)
  | Binop (op, Num e1, Num e2) -> (match op with
    | Sum -> Some (Num (e1 + e2), mem, inp, out)
    (* TODO: implementar outras operações *)
    | _ -> None)

  (* OP2 *)
  | Binop (op, e1, e2) when is_value e1 -> (match step e2 mem inp out with
    | Some (e2', mem', inp', out') -> Some (Binop (op, e2', e2), mem', inp', out')
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

  (* E-LET1 *)
  | Let (x, t, e1, e2) -> (match step e1 mem inp out with
    | Some (e1', mem', inp', out') -> Some (Let (x, t, e1', e2), mem', inp', out')
    | _ -> None)

  (* TODO: E-LET2 *)

  (* ATR1 *)
  | Asg (Id e1, e2) when is_value e2 && Hashtbl.mem mem e1 -> Hashtbl.replace mem e1 e2; Some (Unit, mem, inp, out)

  (* ATR2 *)
  | Asg (Id e1, e2) -> (match step e2 mem inp out with
    | Some (e2', mem', inp', out') -> Some (Asg (Id e1, e2'), mem', inp', out')
    | _ -> None)

  (* ATR *)
  | Asg (e1, e2) -> (match step e1 mem inp out with
    | Some (e1', mem', inp', out') -> Some (Asg (e1', e2), mem', inp', out')
    | _ -> None)

  (* TODO: DEREF1 *)

  (* TODO: DEREF *)

  (* TODO: NEW1 *)

  (* TODO: NEW *)

  (* TODO: SEQ1 *)

  (* TODO: SEQ *)

  (* E-WHILE *)
  | Wh (e1, e2) -> Some (If (e1, Seq (e2, Wh (e1, e2)), Unit), mem, inp, out)

  (* TODO: PRINT-N *)

  (* TODO: PRINT *)

  (* TODO: READ *)

  | _ -> None



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
