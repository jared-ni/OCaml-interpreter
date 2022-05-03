open CS51Utils ;;
open Expr ;;
open Evaluation ;;
open Evaluation.Env ;;
open Absbook ;;


let eval_s_test () = 
    let env = Env.empty () in 
    unit_test (eval_s (Num 1) env 
        = (Val (Num 1))) 
        "eval_s num 1" ;
    unit_test (eval_s (Bool true) env 
        = (Val (Bool true))) 
        "eval_s num 2" ;
    unit_test (eval_s (Fun ("x", Var ("x"))) env 
        = Val (Fun ("x", Var ("x"))))
        "eval_s num 3" ;
    unit_test (try eval_s (Var "x") env = Val (Var "x") with 
        | EvalError "unbound variable" -> true
        | _ -> false) 
        "eval_s num 4" ;
    unit_test (eval_s (Unop (Negate, Num 1)) env 
        = Val (Num ~-1))
        "eval_s num 5" ;
    unit_test (try eval_s (Unop (Negate, Bool true)) env = Val (Var "x") with 
        | EvalError "can't negate non-integers" -> true
        | _ -> false) 
        "eval_s num 6" ;
    unit_test (eval_s (((Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), Num(1),
               Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1)))))),
               App(Var("f"), Num(4)))))) env 
        = Val(Num(24)))
        "eval_s num 7" ;
    unit_test (eval_s ((Let("f", Fun("x", Var("x")), App(App(Var("f"), Var("f")), Num(3))))) env 
        = Val (Num 3))
        "eval_s num 8" ;
    unit_test (eval_s (Binop (Times, Num 12, Num 10)) env 
        = Val (Num 120))
        "eval_s num 9" ;
    unit_test (eval_s (Binop (Minus, Num 12, Num 10)) env 
        = Val (Num 2))
        "eval_s num 10" ;
;;

let eval_d_test () = 
    let env = Env.empty () in 
    unit_test (eval_d (Num 1) env 
        = (Val (Num 1))) 
        "eval_d num 1" ;
    unit_test (eval_d (Bool true) env 
        = (Val (Bool true))) 
        "eval_d num 2" ;
    unit_test (eval_d (Fun ("x", Var ("x"))) env 
        = Val (Fun ("x", Var ("x"))))
        "eval_d num 3" ;
    unit_test (try eval_d (Var "x") env = Val (Var "x") with 
        | EvalError "variable not found in environment" -> true
        | _ -> false) 
        "eval_d num 4" ;
    unit_test (eval_d (Unop (Negate, Num 1)) env 
        = Val (Num ~-1))
        "eval_d num 5" ;
    unit_test (try eval_d (Unop (Negate, Bool true)) env = Val (Var "x") with 
        | EvalError "can't negate non-integers" -> true
        | _ -> false) 
        "eval_d num 6" ;
    unit_test (eval_d (((Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), Num(1),
               Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1)))))),
               App(Var("f"), Num(4)))))) env 
        = Val(Num(24)))
        "eval_d num letrec 7" ;
    unit_test (eval_d ((Let("f", Fun("x", Var("x")), App(App(Var("f"), Var("f")), Num(3))))) env 
        = Val (Num 3))
        "eval_d num 8" ;
    unit_test (eval_d (Binop (Times, Num 12, Num 10)) env 
        = Val (Num 120))
        "eval_d num 9" ;
    unit_test (eval_d (Binop (Minus, Num 12, Num 10)) env 
        = Val (Num 2))
        "eval_d num 10" ;
    unit_test (eval_d (((Let("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), Num(1),
               Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1)))))),
               App(Var("f"), Num(4)))))) env 
        = Val(Num(24)))
        "eval_d num let 11" ;
;;

let test_all () = 
    eval_s_test () ;
    eval_d_test () ;;



let _ = test_all () ;;