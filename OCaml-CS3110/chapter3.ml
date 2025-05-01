open OUnit2

(* Prelininaries *)
let make_test name expected input printer = 
    name >:: (fun _ -> assert_equal expected input ~printer:printer)

(* List Expressions *)
let _lst1 = [1; 2; 3; 4; 5]
let _lst2 = 1 :: 2 :: 3 :: 4 :: 5 :: []
let _lst3 = [1] @ [2; 3; 4] @ [5]

(* Product *)
let product lst = 
    let rec product_acc acc = function
        | [] -> acc 
        | h :: t -> product_acc (acc * h) t 
    in product_acc 1 lst

let make_product_test name expected input =
    make_test name expected input string_of_int

(* Concat *)
let concat lst = 
    let rec concat_acc acc = function
        | [] -> acc 
        | h :: t -> concat_acc (acc ^ h) t
    in concat_acc "" lst 

let make_concat_test name expected input =
    make_test name expected input Fun.id

(* Patterns *)
let bigred = function
    | "bigred" :: _ -> true
    | _ -> false
    
let has_two_or_four = function
    | [_; _] | [_; _; _; _] -> true
    | _ -> false

let make_pattern_test name expected input =
    make_test name expected input string_of_bool 

(* Exercise Tests *)
let tests = "test suite for chapter3" >::: [
    (* Product *)
    make_product_test "empty product" 1 (product []);
    make_product_test "singleton product" 5 (product [5]);
    make_product_test "multiple product" 63 (product [7; 9]);
    make_product_test "negative product" (-6) (product [3; -2]);

    (* Concat *)
    make_concat_test "empty concat" "" (concat []);
    make_concat_test "singleton concat" "test" (concat ["test"]);
    make_concat_test "multiple concat" "test me please" (concat ["test"; " me"; " please"]);

    (* Patterns *)
    make_pattern_test "has bigred in singleton" true (bigred ["bigred"]);
    make_pattern_test "has bigred in multi list" true (bigred ["bigred"; "hello"]);
    make_pattern_test "does not have bigred" false (bigred ["hello"; "world"]);
    make_pattern_test "does not have bigred in first position" false (bigred ["hello"; "bigred"]);
    make_pattern_test "bigred empty list" false (bigred []);

    make_pattern_test "has two elements" true (has_two_or_four [1; 2]);
    make_pattern_test "has four elements" true (has_two_or_four [true; false; false; true]);
    make_pattern_test "does not have two or four elements" false (has_two_or_four ["hello"; "world"; "test"]);
    make_pattern_test "empty list (not two or four)" false (has_two_or_four []);
]

let _ = run_test_tt_main tests
