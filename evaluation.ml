(* 
                         CS 51 Final Project
                         MiniML -- Evaluation
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)

open Expr ;;
  
(* Exception for evaluator runtime, generated by a runtime error in
   the interpreter *)
exception EvalError of string ;;
  
(* Exception for evaluator runtime, generated by an explicit `raise`
   construct in the object language *)
exception EvalException ;;

(*......................................................................
  Environments and values 
 *)

module type ENV = sig
    (* the type of environments *)
    type env
    (* the type of values stored in environments *)
    type value =
      | Val of expr
      | Closure of (expr * env)
   
    (* empty () -- Returns an empty environment *)
    val empty : unit -> env

    (* close expr env -- Returns a closure for `expr` and its `env` *)
    val close : expr -> env -> value

    (* lookup env varid -- Returns the value in the `env` for the
       `varid`, raising an `Eval_error` if not found *)
    val lookup : env -> varid -> value

    (* extend env varid loc -- Returns a new environment just like
       `env` except that it maps the variable `varid` to the `value`
       stored at `loc`. This allows later changing the value, an
       ability used in the evaluation of `letrec`. To make good on
       this, extending an environment needs to preserve the previous
       bindings in a physical, not just structural, way. *)
    val extend : env -> varid -> value ref -> env

    (* env_to_string env -- Returns a printable string representation
       of environment `env` *)
    val env_to_string : env -> string
                                 
    (* value_to_string ?printenvp value -- Returns a printable string
       representation of a value; the optional flag `printenvp`
       (default: `true`) determines whether to include the environment
       in the string representation when called on a closure *)
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : ENV =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    let empty () : env = []

    let close (exp : expr) (env : env) : value =
      Closure (exp, env) 

    let rec lookup (env : env) (varname : varid) : value =
      match env with 
      | [] -> raise (EvalError "variable not found in environment")
      | (vid, valref) :: tl -> 
        if vid = varname then !valref 
        else lookup tl varname

    let rec extend (env : env) (varname : varid) (loc : value ref) : env =
      match env with 
      | [] -> (varname, loc) :: env
      | (vid, valref) :: tl -> 
        if varname = vid then (vid, loc) :: tl 
        else (vid, valref) :: (extend tl varname loc)

    let rec value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with 
      | Val exp -> exp_to_concrete_string exp 
      | Closure (exp, env) -> 
        let exp_str = exp_to_concrete_string exp in 
        if printenvp then 
          "(" ^ exp_str ^ "), " ^ env_to_string env
        else exp_str

    and env_to_string (env : env) : string =
      match env with 
      | [] -> ""
      | (vid, valref) :: tl -> 
        "(" ^ vid ^ ", " ^ value_to_string !valref ^ ");" ^ env_to_string tl
  end
;;


(*......................................................................
  Evaluation functions

  Each of the evaluation functions below evaluates an expression `exp`
  in an environment `env` returning a result of type `value`. We've
  provided an initial implementation for a trivial evaluator, which
  just converts the expression unchanged to a `value` and returns it,
  along with "stub code" for three more evaluators: a substitution
  model evaluator and dynamic and lexical environment model versions.

  Each evaluator is of type `expr -> Env.env -> Env.value` for
  consistency, though some of the evaluators don't need an
  environment, and some will only return values that are "bare
  values" (that is, not closures). 

  DO NOT CHANGE THE TYPE SIGNATURES OF THESE FUNCTIONS. Compilation
  against our unit tests relies on their having these signatures. If
  you want to implement an extension whose evaluator has a different
  signature, implement it as `eval_e` below.  *)

(* The TRIVIAL EVALUATOR, which leaves the expression to be evaluated
   essentially unchanged, just converted to a value for consistency
   with the signature of the evaluators. *)
   
let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp ;;

(* abstracted functions *)
let unop_eval (uno : unop) (exp1 : expr) = 
  match uno, exp1 with 
  | Negate, Num x1 -> Num (~-(x1))
  | Negate, _ -> raise (EvalError "can't negate non-integers")
  | FloatNegate, Float x1 -> Float (~-.(x1))
  | FloatNegate, _ -> raise (EvalError "can't floatNegate non-floats") ;;

let binop_eval (bin : binop) (exp1 : expr) (exp2 : expr) = 
  (* extend binop to float, string concat *)
  match bin, exp1, exp2 with 
  | FloatPlus, Float x1, Float x2 -> Float (x1 +. x2)
  | FloatPlus, _, _ -> raise (EvalError "can't floatPlus non-floats")
  | FloatMinus, Float x1, Float x2 -> Float (x1 -. x2)
  | FloatMinus, _, _ -> raise (EvalError "can't floatMinus non-floats")
  | FloatTimes, Float x1, Float x2 -> Float (x1 *. x2)
  | FloatTimes, _, _ -> raise (EvalError "can't floatTimes non-floats")
  | Plus, Num x1, Num x2 -> Num (x1 + x2)
  | Plus, _, _ -> raise (EvalError "can't add non-integers")
  | Minus, Num x1, Num x2 -> Num (x1 - x2)
  | Minus, _, _ -> raise (EvalError "can't subtract non-integers")
  | Times, Num x1, Num x2 -> Num (x1 * x2) 
  | Times, _, _ -> raise (EvalError "can't multiply non-integers")
  | Equals, Num x1, Num x2 -> Bool (x1 = x2)
  | Equals, Float x1, Float x2 -> Bool (x1 = x2)
  | Equals, Bool x1, Bool x2 -> Bool (x1 = x2)
  | Equals, _, _ -> raise (EvalError "can't compare non-(bool & int)")
  | LessThan, Num x1, Num x2 -> Bool (x1 < x2)
  | LessThan, Float x1, Float x2 -> Bool (x1 < x2)
  | LessThan, Bool x1, Bool x2 -> Bool (x1 > x2)
  | LessThan, _, _ -> raise (EvalError "can't divide non-integers")
  | Concat, String x1, String x2 -> String (x1 ^ x2)
  | Concat, _, _ -> raise (EvalError "can't concatenate non-strings") ;;

(* gets expression from a Env.Val *)
let get_exp (exp_val : Env.value) = 
    match exp_val with 
    | Env.Val v -> v
    | _ -> raise (EvalError "can't get expression from value") ;;

(* The SUBSTITUTION MODEL evaluator -- to be completed *)
let eval_s (exp : expr) (_env : Env.env) : Env.value =
  let rec eval_s_rec (exp : expr) : expr = 
    match exp with 
    | Num _ | Bool _ | Float _ | FunUnit _
    | Fun _ | Unit   | String  _ -> exp
    | Var _ -> raise (EvalError "unbound variable")
    | Binop (bin, exp1, exp2) -> 
      let ex1 = eval_s_rec exp1 in 
      let ex2 = eval_s_rec exp2 in binop_eval bin ex1 ex2
    | Unop (uno, exp1) -> 
      let ex1 = eval_s_rec exp1 in unop_eval uno ex1
    | Conditional (exp1, exp2, exp3) -> 
      (match eval_s_rec exp1 with 
      | Bool b -> 
        if b then eval_s_rec exp2 else eval_s_rec exp3
      | _ -> raise (EvalError "conditional can only evaluate bool"))
    | App (f, a) -> 
        (match eval_s_rec f with 
        | Fun (exp1, exp2) -> 
          eval_s_rec (subst exp1 (eval_s_rec a) exp2)
        | FunUnit (_, exp1) -> 
          (match a with 
          | Unit -> eval_s_rec exp1
          | _ -> raise (EvalError "unit function can only take unit as arg"))
        | _ -> raise (EvalError "failed App "))
    | Let (v, q, r) -> 
          eval_s_rec (subst v (eval_s_rec q) r)
    | Letrec (x, d, b) -> 
          eval_s_rec (subst x (subst x (Letrec (x, d, Var x)) d) b)
    | Raise -> raise (EvalError "evaluation error")
    | Unassigned -> raise (EvalError "unassigned variable")
  in Env.Val (eval_s_rec exp) ;;
     
(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)
let rec eval_d (exp : expr) (env : Env.env) : Env.value =
  let open Env in 
  match exp with 
  | Float _ | String _ | Unit | FunUnit _
  | Num _   | Bool _   | Fun _ -> Val exp
  | Var x -> lookup env x
  | Raise -> raise (EvalError "evaluation error")
  | Unassigned -> raise (EvalError "unassigned variable")
  | Binop (bin, exp1, exp2) -> 
      let ex1 = get_exp (eval_d exp1 env) in 
      let ex2 = get_exp (eval_d exp2 env) in Val(binop_eval bin ex1 ex2)
  | Unop (uno, exp1) -> 
      let ex1 = get_exp (eval_d exp1 env) in Val(unop_eval uno ex1)
  | Conditional (exp1, exp2, exp3) -> 
    (match get_exp (eval_d exp1 env) with 
    | Bool b -> 
        if b then eval_d exp2 env else eval_d exp3 env
    | _ -> raise (EvalError "conditional can only evaluate bool"))
  | App (f, a) -> 
      (match eval_d f env with 
      | Val (Fun (exp1, exp2)) -> 
        eval_d exp2 (extend env exp1 (ref (eval_d a env)))
      | Val (FunUnit (_, exp1)) -> 
        (match a with 
        | Unit -> eval_d exp1 env
        | _ -> raise (EvalError "unit function can only take unit as arg"))
      | _ -> raise (EvalError "failed App "))
  | Let (x, def, body) | Letrec (x, def, body) -> 
        eval_d body (extend env x (ref (eval_d def env))) ;;
       
(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)
   
let rec eval_l (exp : expr) (env : Env.env) : Env.value =
  let open Env in 
  match exp with 
  | Float _ | String _ | Unit
  | Num _   | Bool _   -> Val exp
  | Fun _   | FunUnit _ -> close exp env
  | Var x -> lookup env x
  | Unassigned -> raise (EvalError "unassigned variable")
  | Raise -> raise (EvalError "evaluation error")
  | Binop (bin, exp1, exp2) -> 
      let ex1 = get_exp (eval_l exp1 env) in 
      let ex2 = get_exp (eval_l exp2 env) in Val(binop_eval bin ex1 ex2)
  | Unop (uno, exp1) -> 
      let ex1 = get_exp (eval_d exp1 env) in Val(unop_eval uno ex1)
  | Conditional (exp1, exp2, exp3) -> 
    (match get_exp (eval_l exp1 env) with 
    | Bool b -> 
        if b then eval_l exp2 env else eval_l exp3 env
    | _ -> raise (EvalError "conditional can only evaluate bool"))
  | App (exp_fun, exp_arg) -> 
      (match eval_l exp_fun env with 
      | Closure (v, env_og) -> 
        (match v with 
        | Fun (intp, outp) -> 
          eval_l outp (extend env_og intp (ref (eval_l exp_arg env)))
        | FunUnit (_, exp1) -> 
          (match exp_arg with 
          | Unit -> eval_d exp1 env_og
          | _ -> raise (EvalError "unit function can only take unit as arg"))
        | _ -> raise (EvalError "eval_l: no function application"))
      | _ -> raise (EvalError "eval_l: failed function application"))
  | Let (x, def, body) -> 
        eval_l body (extend env x (ref (eval_l def env)))
  | Letrec (x, def, body) -> 
         let temp : value ref = ref (Val(Unassigned)) in 
         let env_x = extend env x (temp) in 
         let v_D = eval_l def env_x in 
         temp := v_D; eval_l body env_x ;;

(* The EXTENDED evaluator -- if you want, you can provide your
   extension as a separate evaluator, or if it is type- and
   correctness-compatible with one of the above, you can incorporate
   your extensions within `eval_s`, `eval_d`, or `eval_l`. *)

let eval_e (_exp : expr) (_env : Env.env) : Env.value =
  failwith "extensions implemented with the other evaluators" ;;
  
(* Connecting the evaluators to the external world. The REPL in
   `miniml.ml` uses a call to the single function `evaluate` defined
   here. Initially, `evaluate` is the trivial evaluator `eval_t`. But
   you can define it to use any of the other evaluators as you proceed
   to implement them. (We will directly unit test the four evaluators
   above, not the `evaluate` function, so it doesn't matter how it's
   set when you submit your solution.) *)
   
let evaluate = eval_s ;;
let evaluate_d = eval_d ;;
let evaluate_l = eval_l ;;

