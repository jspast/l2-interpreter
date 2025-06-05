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

(* typeInfer : expr -> tipo option *)
let rec typeInfer (e:expr) : tipo option =
  match e with
  | Num n -> Some TyInt
  | Bool b -> Some TyBool
  | Id e' -> None (* TODO: tipo de um endereço *)
  | If (e1, e2, e3) -> (match typeInfer e1 with
    | Some TyBool -> (match typeInfer e2, typeInfer e3 with
      | Some t2, Some t3 -> if t2=t3
        then Some t2 else None
      | _ -> None)
    | _  -> None)
  | Binop (op, e1, e2) -> (match op with
    | Sum | Sub | Mul | Div -> (match typeInfer e1, typeInfer e2 with
      | Some TyInt, Some TyInt -> Some TyInt
      | _ -> None)
    | Eq  | Neq | Lt | Gt | And | Or -> (match typeInfer e1, typeInfer e2 with
      | Some TyBool, Some TyBool -> Some TyBool
      | _ -> None))
  | Wh (e1, e2) -> (match typeInfer e1, typeInfer e2 with
    | Some TyBool, Some TyUnit -> Some TyUnit
    | _ -> None)
  | Asg (e1, e2) -> (match typeInfer e1, typeInfer e2 with
    | Some TyRef t1, Some t2 -> if t1=t2
      then Some TyUnit else None
    | _ -> None)
  | Let (x, t, e1, e2) -> (match t, typeInfer e1, typeInfer e2 with
    | t, Some t1, Some t2 -> if t1=t
      then Some t2 else None
    | _ -> None)
  | New e -> (match typeInfer e with
    | Some t -> Some t
    | _ -> None)
  | Deref e -> (match typeInfer e with
    | Some TyRef t -> Some t
    | _ -> None)
  | Unit -> Some TyUnit
  | Seq (e1, e2) -> (match typeInfer e1, typeInfer e2 with
    | Some TyUnit, Some t -> Some t
    | _ -> None)
  | Read -> Some TyInt
  | Print e -> (match typeInfer e with
    | Some TyInt -> Some TyUnit
    | _ -> None)





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
