(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
  | FloatNegate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
  | Concat
  | FloatPlus
  | FloatMinus
  | FloatTimes
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | FunUnit of unit * expr
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
  (* additional atomic types *)
  | Float of float
  | String of string
  | Unit
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  match exp with 
    (* additional atomic types *)
  | Float _ | String _ | Unit | Num _ | Bool _ -> SS.empty
  | Var v -> SS.singleton v
  | Unop (_, exp1) -> free_vars exp1
  | Binop (_, exp1, exp2) -> SS.union (free_vars exp1) (free_vars exp2)
  | Conditional (exp1, exp2, exp3) -> 
                SS.union (free_vars exp1)
               (SS.union (free_vars exp2) (free_vars exp3))
  | Fun (v, exp1) -> SS.remove v (free_vars exp1)
  | FunUnit (_, exp1) -> free_vars exp1
  | Let (v, exp1, exp2) -> 
        SS.union (SS.remove v (free_vars exp2)) (free_vars exp1)
  | Letrec (v, exp1, exp2) -> 
           SS.union (SS.remove v (free_vars exp2)) 
                    (SS.remove v (free_vars exp1))
  | Raise -> SS.empty
  | Unassigned -> SS.empty (* Should unassigned be added? *)
  | App (exp1, exp2) -> SS.union (free_vars exp1) (free_vars exp2)
;;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let new_varname : unit -> varid =
  let suffix = ref 0 in 
  fun () -> let symbol = "var" ^ string_of_int !suffix in 
            suffix := !suffix + 1;
            symbol ;;
      
(*
let gensym : string -> string =
  let suffix = ref 0 in
  fun str -> let symbol = str ^ string_of_int !suffix in
             suffix := !suffix + 1;
             symbol ;; *)
(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.

  substitution operates eagerly
  1. substitution
  2. evaluation
  environment operates lazily 
  1. evaluation
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let sub = subst var_name repl in
  match exp with 
  | Var v -> if v = var_name then repl else Var v
  (* additional atomic types *)
  | Float _ | String _ | Unit | Num _ 
  | Bool _  | Raise    | Unassigned -> exp
  | Unop (neg, exp1) -> Unop (neg, sub exp1)
  | Binop (bin, exp1, exp2) -> 
          Binop (bin, sub exp1, 
                      sub exp2)
  | Conditional (exp1, exp2, exp3) -> 
                Conditional (sub exp1, sub exp2, sub exp3)
  | App (f, a) -> App (sub f, sub a)
  | FunUnit (u, exp1) -> FunUnit(u, sub exp1)
  | Fun (v, exp1) ->
        if v = var_name then Fun (v, exp1)
        else if not (SS.mem v (free_vars repl)) 
           then Fun (v, sub exp1)
        else let new_var = new_varname () in 
                Fun (new_var, sub
                             (subst v (Var new_var) exp1))
  | Let (v, q, r) -> 
        if v = var_name then Let (v, subst v repl q, r)
        else if not (SS.mem v (free_vars repl))
           then Let (v, sub q, sub r)
        else let new_var = new_varname () in 
                Let (new_var, sub q, 
                              sub (subst v (Var new_var) r))
  | Letrec (v, q, r) -> 
           (* do nothing, because x is not free variable *)
           if v = var_name then Letrec (v, q, r)
           else if not (SS.mem v (free_vars repl))
            then Letrec (v, sub q, sub r)
           (* sub for both Q and P *)
           else let new_var = new_varname () in 
                let z = Var new_var in 
                Letrec (new_var, sub (subst v z q),
                                 sub (subst v z r))
;;


(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with 
  | Var v -> v
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unop (Negate, exp) -> "~-(" ^ exp_to_concrete_string exp ^ ")"
  | Unop (FloatNegate, exp) -> "~-.(" ^ exp_to_concrete_string exp ^ ")"
  | Binop (bin, exp1, exp2) -> 
          let bin_str : string = 
            match bin with 
            | Plus -> " + "
            | Minus -> " - "
            | Times -> " * "
            | Equals -> " = "
            | LessThan -> " < " 
            (* added float and string support *)
            | Concat -> " ^ "
            | FloatPlus -> " +. "
            | FloatMinus -> " -. "
            | FloatTimes -> " *. "
            in
          "(" ^ (exp_to_concrete_string exp1) ^ bin_str ^ 
          (exp_to_concrete_string exp2) ^ ")"
  | Conditional (exp1, exp2, exp3) -> 
                "if " ^ (exp_to_concrete_string exp1) ^ 
                " then " ^ (exp_to_concrete_string exp2) ^
                " else " ^ (exp_to_concrete_string exp3)
  | Fun (v, exp) -> 
        "fun " ^ v ^ " -> " ^ exp_to_concrete_string exp
  | FunUnit (_, exp) -> 
        "fun () -> " ^ exp_to_concrete_string exp
  | Let (v, exp1, exp2) -> 
        "let " ^ v ^ " = " ^ 
        exp_to_concrete_string exp1 ^ " in " ^ 
        exp_to_concrete_string exp2
  | Letrec (v, exp1, exp2) -> 
        "let rec " ^ v ^ " = " ^ 
        exp_to_concrete_string exp1 ^ " in " ^
        exp_to_concrete_string exp2
  | Raise -> "Exception "
  | Unassigned -> "Unassigned "
  | App (exp1, exp2) -> exp_to_concrete_string exp1 ^ " " ^
                        exp_to_concrete_string exp2
  (* additional atomic types *)
  | Float f -> string_of_float f
  | String s -> "\"" ^ s ^ "\""
  | Unit -> "()"
  ;;
     

(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with 
  | Var v -> "Var(" ^ v ^ ")"
  | Num n -> "Num(" ^ string_of_int n ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Unop (n, exp) -> 
          (match n with 
          | Negate -> "Unop(Negate, "
          | FloatNegate -> "Unop(FloatNegate, ") ^ 
                       (exp_to_abstract_string exp) ^ ")"
  | Binop (bin, exp1, exp2) -> 
          let bin_str : string = 
            match bin with 
            | Plus -> "Plus"
            | Minus -> "Minus"
            | Times -> "Times"
            | Equals -> "Equals"
            | LessThan -> "LessThan" 
            | Concat -> "Concat" 
            | FloatPlus -> "Floatplus"
            | FloatMinus -> "FloatMinus"
            | FloatTimes -> "FloatTimes" in
          "Binop(" ^ bin_str ^ ", " ^ exp_to_abstract_string exp1 ^ ", " ^
                                      exp_to_abstract_string exp2 ^ ")"
  | Conditional (exp1, exp2, exp3) -> 
                "Conditional(" ^ 
                exp_to_abstract_string exp1 ^ ", " ^
                exp_to_abstract_string exp2 ^ ", " ^
                exp_to_abstract_string exp3 ^ ")"
  | Fun (v, exp) -> 
        "Fun(" ^ v ^ ", " ^ exp_to_abstract_string exp ^ ")"
  | FunUnit (_, exp) -> 
        "FunUnit(unit, " ^ exp_to_abstract_string exp ^ ")"
  | Let (v, exp1, exp2) -> 
        "Let(" ^ v ^ ", " ^ exp_to_abstract_string exp1 ^ ", " ^
                            exp_to_abstract_string exp2 ^ ")"
  | Letrec (v, exp1, exp2) -> 
           "Letrec(" ^ v ^ ", " ^ exp_to_abstract_string exp1 ^ ", " ^
                                  exp_to_abstract_string exp2 ^ ")"
  | Raise -> "Raise()"
  | Unassigned -> "Unassigned"
  | App (exp1, exp2) -> 
        "App(" ^ exp_to_abstract_string exp1 ^ ", " ^
                 exp_to_abstract_string exp2 ^ ")" 
  (* additional atomic types *)
  | Float f -> "Float(" ^ string_of_float f ^ ")"
  | String s -> "String(\"" ^ s ^ "\")"  
  | Unit -> "Unit"
  ;;
