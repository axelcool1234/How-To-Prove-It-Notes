open OUnit2

(* Prelininaries *)
let make_test name expected input printer = 
    name >:: (fun _ -> assert_equal expected input ~printer:printer)

let lst_printer printer lst = List.fold_left (fun str elem -> str ^ (printer elem) ^ "; ") "[" lst ^ "]"

let option_printer printer = function
    | None -> "None" 
    | Some x -> printer x

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

let first_two_eq lst1 lst2 = 
    match (lst1, lst2) with
    | (a :: b :: _), (c :: d :: _) -> a == c && b == d
    | _ -> false

let make_pattern_test name expected input =
    make_test name expected input string_of_bool 

(* Library *)
let fifth_element lst = 
    match List.nth_opt lst 4 with
    | None -> 0
    | Some element -> element

let sort_desc lst =
    lst |> List.sort Stdlib.compare |> List.rev

let make_library_test name expected input =
    make_test name expected input string_of_int

(* Library Puzzle *)
let last_element lst = List.nth lst (List.length lst - 1)
let has_zero lst = List.exists (fun x -> x = 0) lst

(* Take Drop *)
let take n lst = 
    let rec take_acc_rev acc n lst =
        match n, lst with
        | 0, _ | _, [] -> acc 
        | n, x :: xs -> take_acc_rev (x :: acc) (n - 1) xs 
    in lst |> take_acc_rev [] n |> List.rev

let rec drop n lst =
    match n, lst with
    | 0, _ | _, [] -> lst 
    | n, _ :: xs -> drop (n - 1) xs 

(* Unimodal *)
(*let is_unimodal = *)
(*    let rec is_dec prev lst =*)
(*        match prev, lst with*)
(*        | _, [] -> true*)
(*        | prev, x :: xs -> x <= prev && is_dec x xs*)
(*    in let rec is_inc_then_dec prev lst =*)
(*        match prev, lst with*)
(*        | _, [] -> true*)
(*        | prev, x :: xs -> (x >= prev && is_inc_then_dec x xs) || is_dec x xs*)
(*    in function*)
(*        | [] -> true*)
(*        | x :: xs -> is_inc_then_dec x xs*)

let rec is_unimodal = 
    let rec is_dec = function
        | [] | [_] -> true
        | h1 :: (h2 :: _ as t) -> h1 >= h2 && is_dec t
    in function
        | [] | [_] -> true        
        | h1 :: (h2 :: _ as t) -> if h1 <= h2 then is_unimodal t else is_dec t

(* Powerset *)

(* Print Int List Rec *)
let [@warning "-unused-value-declaration"] rec print_int_list = function
    | [] -> ()
    | h :: t -> print_endline (string_of_int h); print_int_list t

(* Print Int List Iter *)
let [@warning "-unused-value-declaration"] print_int_list' = List.iter (fun x -> print_endline (string_of_int x))

(* Student *)
[@@@warning "-unused-value-declaration"]
[@@@warning "-unused-field"]
type student = {first_name : string; last_name : string; gpa : float}

let my_student = {first_name = "John"; last_name = "Doe"; gpa = 3.8}

(*let extract_name = function *)
(*    | {first_name; last_name; _} -> first_name, last_name*)
let extract_name student = student.first_name, student.last_name

let create_student first_name last_name gpa = {first_name; last_name; gpa}

(* Pokerecord *)
type poketype = Normal | Fire | Water

type pokemon = { name : string; hp : int; ptype : poketype }

let charizard = { name = "charizard"; hp = 78; ptype = Fire }
let squirtle = { name = "squirtle"; hp = 44; ptype = Water }
let human = { name = "human"; hp = 5; ptype = Normal }
[@@@warning "+unused-value-declaration"]
[@@@warning "+unused-field"]

(* Safe hd and tl *)
let safe_hd = function
    | [] -> None
    | h :: _ -> Some h

let safe_tl = function
    | [] -> None
    | _ :: t -> Some t

(* Pokefun *)
let max_hp = 
    let rec max_hp_acc acc lst =
        match acc, lst with
        | acc, [] -> acc
        | None, h :: t -> max_hp_acc (Some h) t
        | Some acc, h :: t -> max_hp_acc (if h.hp > acc.hp then Some h else Some acc) t
    in max_hp_acc None

(* Date Before *)

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

    make_pattern_test "first two elements in each list equal" true (first_two_eq [1;2;0] [1;2;9]);
    make_pattern_test "first two elements in each list not equal" false (first_two_eq [2;1;1] [1;2;9]);
    make_pattern_test "empty and singleton (first two elements not equal)" false (first_two_eq [] [1]);

    (* Library *)
    make_library_test "has fifth element" 4 (fifth_element [0;1;2;3;4]);
    make_library_test "does not have fifth element" 0 (fifth_element [0;1;2;3]);
    "to descending" >:: (fun _ -> assert_equal [4;3;2;1;0] (sort_desc [1;2;0;4;3])); 
    "ascending to descending" >:: (fun _ -> assert_equal [4;3;2;1;0] (sort_desc [0;1;2;3;4])); 

    (* Library Puzzle *)
    make_library_test "last element" 2 (last_element [0;1;4;3;2]);
    make_library_test "last element asc" 4 (last_element [0;1;2;3;4]);
    make_test "has zero" true (has_zero [0;1;2;3;4]) string_of_bool;
    make_test "does not have zero" false (has_zero [4;3;5]) string_of_bool;

    (* Take Drop *)
    "take all" >:: (fun _ -> assert_equal [0;1;2] (take 4 [0;1;2]) ~printer: (lst_printer string_of_int));
    "take some" >:: (fun _ -> assert_equal [0;1] (take 2 [0;1;2]) ~printer: (lst_printer string_of_int));
    "drop all" >:: (fun _ -> assert_equal [] (drop 4 [0;1;2]) ~printer: (lst_printer string_of_int));
    "drop some" >:: (fun _ -> assert_equal [2] (drop 2 [0;1;2]) ~printer: (lst_printer string_of_int));

    (* Unimodal *)
    make_test "is unimodal" true (is_unimodal [0;1;2;2;3;3;2;1;1;0]) string_of_bool;
    make_test "is unimodal (empty list)" true (is_unimodal []) string_of_bool;
    make_test "is unimodal (singleton)" true (is_unimodal [5]) string_of_bool;
    make_test "is not unimodal" false (is_unimodal [0;1;2;2;3;3;2;4;1;0]) string_of_bool;
    make_test "is unimodal big list (testing for tail recursion)" true (is_unimodal (List.init 100_000_000 Fun.id)) string_of_bool;

    (* Safe hd and tl *)
    make_test "head non-empty" (Some 5) (safe_hd [5;4;3;2;1;0]) (option_printer string_of_int);
    make_test "head empty" None (safe_hd []) (option_printer string_of_int);
    make_test "tail non-empty" (Some [4;3;2;1;0]) (safe_tl [5;4;3;2;1;0]) (option_printer (lst_printer string_of_int));
    make_test "tail empty" None (safe_tl []) (option_printer (lst_printer string_of_int));

    (* Pokefun *)
    "Charizard Pokemon" >:: (fun _ -> assert_equal (Some charizard) (max_hp [squirtle; charizard])); 
    "Empty Pokemon" >:: (fun _ -> assert_equal None (max_hp [])); 
]

let _ = run_test_tt_main tests
