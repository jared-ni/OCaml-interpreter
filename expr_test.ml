open CS51Utils ;;
open Expr ;;
open Absbook ;;

let exp_to_concrete_string_test () = 
    unit_test (exp_to_concrete_string (Num 3) = "3")
        "exp_to_concrete_string 3" ;
    unit_test (exp_to_concrete_string (Var "var") = "var")
        "exp_to_concrete_string var" ;
    unit_test (exp_to_concrete_string (Bool true) = "true")
        "exp_to_concrete_string app true" ;
    unit_test (exp_to_concrete_string (Bool false) = "false")
        "exp_to_concrete_string app false" ;
    unit_test (exp_to_concrete_string (Unop (Negate, Num 1)) = "~-(1)")
        "exp_to_concrete_string app -1" ;  
    unit_test (exp_to_concrete_string (Binop (Plus, Num 1, Num 2)) = "(1 + 2)")
        "exp_to_concrete_string app 1 + 2" ;
    unit_test (exp_to_concrete_string (Binop (Minus, Num 1, Num 2)) = "(1 - 2)")
        "exp_to_concrete_string app 1 - 2" ;
    unit_test (exp_to_concrete_string (Binop (Times, Num 1, Num 2)) = "(1 * 2)")
        "exp_to_concrete_string app 1 * 2" ;
    unit_test (exp_to_concrete_string (Binop (Equals, Num 1, Num 2)) = "(1 = 2)")
        "exp_to_concrete_string app 1 = 2" ;
    unit_test (exp_to_concrete_string (Binop (LessThan, Num 1, Num 2)) = "(1 < 2)")
        "exp_to_concrete_string app 1 < 2" ;
    unit_test (exp_to_concrete_string (Conditional (Bool true, Num 1, Num 2)) 
        = "if true then 1 else 2") "exp_to_concrete_string app if then else" ;
    unit_test (exp_to_concrete_string 
              (Let("f", Fun("x", Var("x")), App(App(Var("f"), Var("f")), Num(3)))) = 
              "let f = fun x -> x in f f 3")
        "exp_to_concrete_string app let f = fun x -> x in f f 3" ;
    unit_test (exp_to_concrete_string 
              (Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), Num(1),
               Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1)))))),
               App(Var("f"), Num(4)))) = 
        "let rec f = fun x -> if (x = 0) then 1 else (x * f (x - 1)) in f 4")
        "exp_to_concrete_string let rec f = fun x -> if (x = 0) then 1 else x * f (x - 1) in f 4" ;
;;

let exp_to_abstract_string_test () = 
    unit_test (exp_to_abstract_string (Num 3) = "Num(3)")
        "exp_to_abstract_string 3" ;
    unit_test (exp_to_abstract_string (App(Num(3), Num(4))) = "App(Num(3), Num(4))")
        "exp_to_abstract_string app 3 4" ;
    unit_test (exp_to_abstract_string (Bool true) = "Bool(true)")
        "exp_to_abstract_string app bool true" ;
    unit_test (exp_to_abstract_string (Unop (Negate, Num 1)) = "Unop(Negate, Num(1))")
        "exp_to_abstract_string app Unop" ;
    unit_test (exp_to_abstract_string (Binop (Plus, Num 1, Num 2)) 
        = "Binop(Plus, Num(1), Num(2))") "exp_to_abstract_string app Plus" ;
    unit_test (exp_to_abstract_string (Binop (Minus, Num 1, Num 2)) 
        = "Binop(Minus, Num(1), Num(2))") "exp_to_abstract_string app Minus" ;
    unit_test (exp_to_abstract_string (Binop (Times, Num 1, Num 2)) 
        = "Binop(Times, Num(1), Num(2))") "exp_to_abstract_string app Times" ;
    unit_test (exp_to_abstract_string (Binop (Equals, Num 1, Num 2)) 
        = "Binop(Equals, Num(1), Num(2))") "exp_to_abstract_string app Equal" ;
    unit_test (exp_to_abstract_string (Binop (LessThan, Num 1, Num 2)) 
        = "Binop(LessThan, Num(1), Num(2))") "exp_to_abstract_string app Less" ;
    unit_test (exp_to_abstract_string (Fun ("x", Var "x")) 
        = "Fun(x, Var(x))") "exp_to_abstract_string app Fun" ;
    unit_test (exp_to_abstract_string 
              (Let("f", Fun("x", Var("x")), App(App(Var("f"), Var("f")), Num(3)))) = 
              "Let(f, Fun(x, Var(x)), App(App(Var(f), Var(f)), Num(3)))")
        "exp_to_abstract_string Let(f, Fun(x, Var(x)), App(App(Var(f), Var(f)), Num(3)))" ;
    unit_test (exp_to_abstract_string 
              (Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), Num(1),
               Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1)))))),
               App(Var("f"), Num(4)))) = 
              "Letrec(f, Fun(x, Conditional(Binop(Equals, Var(x), Num(0)), Num(1), Binop(Times, Var(x), App(Var(f), Binop(Minus, Var(x), Num(1)))))), App(Var(f), Num(4)))") 
              "exp_to_abstract_string \
               Letrec(f, Fun(x, Conditional(Binop(Equals, Var(x), Num(0)), Num(1),\
                Binop(Times, Var(x), App(Var(f), Binop(Minus, Var(x), Num(1)))))),\
                App(Var(f), Num(4)))" ;
;;


let free_vars_test () = 
    unit_test (same_vars (vars_of_list []) (free_vars (Bool true))) "free_vars bool" ;
    unit_test (same_vars (vars_of_list []) (free_vars (Num 1))) "free_vars 1" ;
    unit_test (same_vars (vars_of_list []) (free_vars (Unassigned))) "free_vars unassigned" ;
    unit_test (same_vars (vars_of_list ["x"]) 
              (free_vars (Binop (Minus, Var("x"), Num(1))))) 
              "free_vars Minus" ;
    unit_test (same_vars (vars_of_list ["x"; "y"]) 
              (free_vars (Binop (Times, Var("x"), Var("y"))))) 
              "free_vars x y" ;
    unit_test (same_vars (vars_of_list ["x"; "y"]) 
              (free_vars (Binop (Times, Unop(Negate, Var("x")), Var("y"))))) 
              "free_vars x y" ;
    unit_test (same_vars (vars_of_list ["x"]) (free_vars (Var("x")))) "free_vars x" ;
    unit_test (same_vars 
              (free_vars (Let("f", Fun("x", Var("x")), App(App(Var("f"), Var("f")), Num(3)))))
              (vars_of_list [])
    ) "free_vars none" ;
    unit_test (same_vars 
              (free_vars (Let("f", Fun("x", Var("x")), App(App(Var("f"), Var("f")), Num(3)))))
              (vars_of_list [])
    ) "free_vars none" ;
    unit_test (same_vars
              (free_vars ((Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), Num(1),
               Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1)))))),
               App(Var("f"), Num(4))))))
              (vars_of_list [])
    ) "free_vars long f"
;;

let test_all () = 
    exp_to_concrete_string_test ();
    exp_to_abstract_string_test ();
    free_vars_test ();
    print_endline "done"
;;

let _ = test_all () ;;