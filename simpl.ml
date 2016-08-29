open Simpltypes;;

type vartyp =
        Undeclared
      | VTyp of (ityp * bool)

type typctx = varname -> vartyp

type cmdtyp = TypCtx of typctx | CTypErr of string

type exprtyp = ExpTyp of ityp | ETypErr of string

let update s v i = (fun x -> if x=v then i else (s x));;

let init_typctx (l : (varname*vartyp) list) : typctx =
  fun x -> (try (List.assoc x l) with Not_found -> Undeclared);;

let print_lineinfo (s: string) (li: lineinfo) = 
  "Error due to "^ s ^" starting at line "^string_of_int(fst(fst(li)))^" column "^string_of_int(snd(fst(li)))^" ending at line "^string_of_int(fst(snd(li)))^" column "^string_of_int(snd(snd(li)));;

let rec typchk_expr (tc:typctx) (e:iexpr) : exprtyp = 
  let typchk_op (a:ityp) (r:ityp) (e1:iexpr) (e2:iexpr)(li:lineinfo) :exprtyp = 
    match (typchk_expr tc e1) with 
    ExpTyp x -> if x=a 
                  then (match (typchk_expr tc e1) with 
                        ExpTyp x -> if x=a then ExpTyp r else ETypErr (print_lineinfo "Type mismatch with expected type" li)
                        |ETypErr s -> ETypErr s )
                  else ETypErr (print_lineinfo "Type mismatch with expected type" li)
    |ETypErr s -> ETypErr s
  in
  match e with Const (x,y) -> ExpTyp(TypInt)
      | Var (x,y) -> (match (tc x) with VTyp(TypInt,true) -> ExpTyp(TypInt) 
                                      | VTyp(TypBool,true)-> ExpTyp(TypBool) 
                                      | VTyp(_,false)->  ETypErr (print_lineinfo "Invalid access of variable" y)
                                      | Undeclared ->  ETypErr (print_lineinfo "undeclared variable" y))
      | Plus (x,y,z) -> typchk_op TypInt TypInt x y z
      | Minus (x,y,z) -> typchk_op TypInt TypInt x y z
      | Times (x,y,z) -> typchk_op TypInt TypInt x y z
      | True x -> ExpTyp TypBool
      | False x -> ExpTyp TypBool
      | Leq (x,y,z) -> typchk_op TypInt TypBool x y z
      | Conj (x,y,z) -> typchk_op TypBool TypBool x y z
      | Disj (x,y,z) -> typchk_op TypBool TypBool x y z
      | Neg (x,y) -> if (typchk_expr tc x = ExpTyp TypBool) then ExpTyp TypBool else ETypErr (print_lineinfo "Type mismatch with expected type. Expected was bool type" y)
      | Abstraction (x,y,z) -> 
      | Apply (x,y,z) -> match tc x with VTyp
;;

and typchk_cmd (tc:typctx) (c:icmd) : cmdtyp =
  (* YOUR ASSIGNMENT 5 SOLUTION GOES HERE
   * Copy your assignment 5 solution for typchk_cmd into this space (or copy
   * the solution set if your solution didn't work).  This part is unchanged
   * from assignment 5. *)
  CTypErr "not yet implemented";;  (* DELETE THIS LINE *)


(* Your interpreter may throw the SegFault exception if it ever encounters
 * a stuck state. *)
exception SegFault

(* Stores now map variable names to either integers or code. Code consists
 * of a command and a list of the names of the variables it takes as input. *)
type heapval = Data of int | Code of (varname list * icmd)
type store = varname -> heapval

let init_store (l : (varname*heapval) list) : store =
  fun x -> List.assoc x l;;

let rec eval_expr (s:store) (e:iexpr) : heapval =

  let eval_intop f (e1,e2,_) =
    (match (eval_expr s e1, eval_expr s e2) with
       (Data n1, Data n2) -> Data (f n1 n2)
     | _ -> raise SegFault) in

  let eval_boolop f =
    eval_intop (fun x y -> if (f (x<>0) (y<>0)) then 1 else 0) in

  (match e with
     Const (n,_) -> Data n
   | Var (x,_) -> (s x)
   | Plus z -> eval_intop (+) z
   | Minus z -> eval_intop (-) z
   | Times z -> eval_intop ( * ) z
   | True _ -> Data 1
   | False _ -> Data 0
   | Leq z -> eval_intop (fun x y -> if x<=y then 1 else 0) z
   | Conj z -> eval_boolop (&&) z
   | Disj z -> eval_boolop (||) z
   | Neg (e1,li) -> eval_boolop (fun x _ -> not x) (e1,True li,li)
   | Abstraction (al,c,_) -> Code ((List.fold_left (fun b x-> match x with (y,TypInt,_) -> b::y | (y,TypBool,_) -> b::y | _ ->raise SegFault) [] al), c)
   | Apply (e1,el,_) ->  match (s e1) with Code (x,y)  -> (exec_cmd (List.fold_left (fun b h-> update s b (eval_expr s h)) x el) y) "ret" | _ -> raise SegFault
  )

and exec_cmd (s:store) (c:icmd) : store =
  (match c with
     Skip _ | Decl _ -> s
   | Seq (c1,c2,_) -> exec_cmd (exec_cmd s c1) c2
   | Assign (v,e,_) -> update s v (eval_expr s e)
   | Cond (e,c1,c2,_) ->
       exec_cmd s (if (eval_expr s e)=(Data 0) then c2 else c1)
   | While (e,c1,li) -> exec_cmd s (Cond (e,Seq (c1,c,li),Skip li,li))
  );;



let main () =
   let argval = (function "true" -> 1 | "false" -> 0 | x -> int_of_string x) in
   let argtyp = (function "true" | "false" -> TypBool | _ -> TypInt) in
   let c = (Simplparser.parse_cmd Simpllexer.token 
              (Lexing.from_channel (open_in Sys.argv.(1)))) in
   let s = init_store (List.tl (List.tl (Array.to_list (Array.mapi
             (fun i a -> ("arg"^(string_of_int (i-2)),
                          Data (if i>=2 then (argval a) else 0)))
               Sys.argv)))) in
   let tc = init_typctx (List.tl (List.tl (Array.to_list (Array.mapi
             (fun i a -> ("arg"^(string_of_int (i-2)), VTyp (argtyp a,true)))
             Sys.argv)))) in
   (match (typchk_cmd tc c) with
      CTypErr s -> print_string ("Typing error: "^s^"\n")
    | TypCtx tc' -> (print_string
        (match (tc' "ret") with
           Undeclared -> "Typing error: return value undeclared"
         | VTyp(_,false) -> "Typing error: return value uninitialized"
         | VTyp(rtyp,true) ->
             (match (exec_cmd s c "ret") with
                Code _ -> "<code>"
              | Data n -> if rtyp=TypInt then (string_of_int n)
                          else if n=0 then "false" else "true"));
        print_newline ()));;

main ();;

