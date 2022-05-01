open CS51Utils ;;
open Expr ;;
open Absbook ;;

let exp_to_concrete_string_test () = 
    unit_test (exp_to_concrete_string (Num 3) = "3")
        "exp_to_concrete_string 3" ;
    unit_test (exp_to_concrete_string (App(Num(3), Num(4))) = "3 4")
        "exp_to_concrete_string app 3 4" ;
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
    unit_test (exp_to_abstract_string 
              (Let("f", Fun("x", Var("x")), App(App(Var("f"), Var("f")), Num(3)))) = 
              "Let(f, Fun(x, Var(x)), App(App(Var(f), Var(f)), Num(3)))")
        "exp_to_abstract_string Let(f, Fun(x, Var(x)), App(App(Var(f), Var(f)), Num(3)))" ;
    unit_test (exp_to_abstract_string 
              (Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), Num(1),
               Binop(Times, Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1)))))),
               App(Var("f"), Num(4)))) = 
              "Letrec(f, Fun(x, Conditional(Binop(Equals, Var(x), Num(0)), Num(1), Binop(Times, Var(x), App(Var(f), Binop(Minus, Var(x), Num(1)))))), App(Var(f), Num(4)))")
        "exp_to_abstract_string\
               Letrec(f, Fun(x, Conditional(Binop(Equals, Var(x), Num(0)), Num(1),\
                Binop(Times, Var(x), App(Var(f), Binop(Minus, Var(x), Num(1)))))),\
                App(Var(f), Num(4)))" ;
;;


let free_vars_test () = 
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