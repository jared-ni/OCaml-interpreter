open CS51Utils ;;
open Expr ;;
open Evaluation ;;
open Evaluation.Env ;;
open Absbook ;;


let env_test () = 
    let e1 = empty () in
    let e2 = extend e1 "z" (ref (Val (Num 42))) in 
    let e3 = extend e2 "z" (ref (Val (Num 69))) in
    let e4 = 
    unit_test (close (Bool true) e1 
        = Closure(Bool true, e1))
        "env test 1" ;


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
    unit_test (eval_s (String("what")) env 
        = Val(String("what")))
        "eval_s string 11" ;
    unit_test (eval_s (Float(1.2)) env 
        = Val(Float(1.2)))
        "eval_s float 12";
    unit_test (eval_s (Unop(FloatNegate, Float(1.2))) env 
        = Val(Float(~-.1.2)))
        "eval_s float 13";
    unit_test (eval_s (Unop(FloatNegate, Binop(FloatTimes, Float(1.2), Float(2.0)))) env 
        = Val(Float(~-.2.4)))
        "eval_s float 14";
    unit_test (eval_s (Binop(Concat, String("what"), String("heck"))) env 
        = Val(String("whatheck")))
        "eval_s string 15" ;
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
    unit_test (eval_d (String("what")) env 
        = Val(String("what")))
        "eval_d string 12" ;
    unit_test (eval_d (Float(1.2)) env 
        = Val(Float(1.2)))
        "eval_d float 13";
    unit_test (eval_d (Unop(FloatNegate, Float(1.2))) env 
        = Val(Float(~-.1.2)))
        "eval_d float 14";
    unit_test (eval_d (Unop(FloatNegate, Binop(FloatTimes, Float(1.2), Float(2.0)))) env 
        = Val(Float(~-.2.4)))
        "eval_d float 15";
    unit_test (eval_d (Binop(Concat, String("what"), String("heck"))) env 
        = Val(String("whatheck")))
        "eval_d string 16" ;
;;

let eval_l_test () = 
    let env = Env.empty () in 
    unit_test (eval_l (Num 1) env 
        = (Val (Num 1))) 
        "eval_l num 1" ;
    unit_test (eval_l (Bool true) env 
        = (Val (Bool true))) 
        "eval_l num 2" ;
    unit_test (eval_l (Fun ("x", Var ("x"))) env 
        = Env.Closure(Fun ("x", Var ("x")), env))
        "eval_l num 3" ;
    unit_test (try eval_l (Var "x") env = Val (Var "x") with 
        | EvalError _ -> true
        | _ -> false) 
        "eval_l num 4" ;
    unit_test (eval_l (Unop (Negate, Num 1)) env 
        = Val (Num ~-1))
        "eval_l num 5" ;
    unit_test (try eval_l (Unop (Negate, Bool true)) env = Val (Var "x") with 
        | EvalError "can't negate non-integers" -> true
        | _ -> false) 
        "eval_l num 6" ;
    unit_test (eval_l (((Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), Num(1),
               Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1)))))),
               App(Var("f"), Num(4)))))) env 
        = Val(Num(24)))
        "eval_l num 7" ;
    unit_test (eval_l ((Let("f", Fun("x", Var("x")), App(App(Var("f"), Var("f")), Num(3))))) env 
        = Val (Num 3))
        "eval_l num 8" ;
    unit_test (eval_l (Binop (Times, Num 12, Num 10)) env 
        = Val (Num 120))
        "eval_l num 9" ;
    unit_test (eval_l (Binop (Minus, Num 12, Num 10)) env 
        = Val (Num 2))
        "eval_l num 10" ;
    unit_test (eval_l (String("what")) env 
        = Val(String("what")))
        "eval_l string 11" ;
    unit_test (eval_l (Float(1.2)) env 
        = Val(Float(1.2)))
        "eval_l float 12";
    unit_test (eval_l (Unop(FloatNegate, Float(1.2))) env 
        = Val(Float(~-.1.2)))
        "eval_l float 13";
    unit_test (eval_l (Unop(FloatNegate, Binop(FloatTimes, Float(1.2), Float(2.0)))) env 
        = Val(Float(~-.2.4)))
        "eval_l float 14";
    unit_test (eval_l (Binop(Concat, String("what"), String("heck"))) env 
        = Val(String("whatheck")))
        "eval_l string 15" ;
;;

let test_all () = 
    eval_s_test () ;
    eval_d_test () ;
    eval_l_test () ;;



let _ = test_all () ;;