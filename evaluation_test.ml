open CS51Utils ;;
open Expr ;;
open Evaluation ;;
open Absbook ;;


let eval_s_test () = 
    unit_test (eval_s (Var (x)) )


let test_all () = 
    eval_s_test () ;;



let _ = test_all () ;;