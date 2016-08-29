type varname = string

(* There is now a new ityp for functions.  If a function f has arguments with
 * types t1,t2,...,tn and has a return type of rt, then function f has type
 * TypFunc([t1;t2;...;tn],rt). *)
type ityp =
        TypInt
      | TypBool
      | TypFunc of (ityp list * ityp)

(* The lineinfo type is unchanged from the last programming assignment. *)
type lineinfo = (int * int) * (int * int)

(* A SIMPL function's formal parameter consists of its variable name,
 * its type, and the position where it was declared in the source file *)
type iformal = varname * ityp * lineinfo

(* Two new iexpr constructors have been added:
 *   1. (Abstraction (l,c)) is a function abstraction, where l is a list of
 *      iformal values and c is the icmd that forms the body of the function.
 *   2. (Apply (e,l)) is an application of a function to a list of
 *      arguments. Expression e evaluates to the function and l is a
 *      list of expressions that evaluate to the respective arguments
 *      to be passed to the function. *)
type iexpr =
        Const of (int * lineinfo)
      | Var of (varname * lineinfo)
      | Plus of (iexpr * iexpr * lineinfo)
      | Minus of (iexpr * iexpr * lineinfo)
      | Times of (iexpr * iexpr * lineinfo)
      | True of lineinfo
      | False of lineinfo
      | Leq of (iexpr * iexpr * lineinfo)
      | Conj of (iexpr * iexpr * lineinfo)
      | Disj of (iexpr * iexpr * lineinfo)
      | Neg of (iexpr * lineinfo)
      | Abstraction of (iformal list * icmd * lineinfo)
      | Apply of (iexpr * iexpr list * lineinfo)

and icmd =
        Skip of lineinfo
      | Seq of (icmd * icmd * lineinfo)
      | Assign of (varname * iexpr * lineinfo)
      | Cond of (iexpr * icmd * icmd * lineinfo)
      | While of (iexpr * icmd * lineinfo)
      | Decl of (ityp * varname * lineinfo)

