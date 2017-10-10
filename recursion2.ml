  (**
  * CSC 208
  * Homework 5: Recursion and Induction
  *)

  (* A simple assertion library (don't modify this!) {{{ *)

let run_test (desc:string) (f: unit -> bool) =
  print_endline ("Running: " ^ desc ^ "... ");
  flush_all ();
  try if not (f ()) then
    begin
      print_endline "Test failed!"
    end
  else
    ()
  with e -> print_endline ("Test error: " ^ Printexc.to_string e)

      (* }}} *)

      (*
 * Implement each of the recursive functions over lists below as well as their
 * associated test cases.
       *)

      (* drop n l returns l but with the first n elements removed.
       * pre: n >= 0 *)
let rec drop (n:int) (l:'a list) : 'a list =
  if n > 0 then
    match l with
    |[]       -> l
    |x :: xs  -> drop (n-1) xs
  else
    l
let test () : bool =
  drop 1 [1; 2; 3] = [2; 3]
;; run_test "drop: drops one element from the list" test

let test () : bool =
  drop 4 [1; 2; 3] = []
;; run_test "drop: n is greater than the elements of the list so it returns an empty list" test

let test () : bool =
  drop 0 [1; 2; 3] = [1; 2; 3]
;; run_test "drop: n is equal to zero so it'll return the same list" test

(* replicate n v l produces the list containing only v of length n
 * pre: n >= 0 *)
let rec replicate (n:int) (v:'a) (l:'a list) : 'a list =
  if n > 0 then
    match l with
    |l  -> v :: replicate (n-1) v l
  else
    l

let test () : bool =
  replicate 2 "hi" [] = ["hi"; "hi"]
;; run_test "replicate: checking with type string 'hi' replicating it twice" test

let test () : bool =
  replicate 4 0 [] = [0;0;0;0]
;; run_test "replicate: checking with type int '0' replicating it 4 times" test

let test () : bool =
  replicate 4 1 [2; 3 ;4] = [1; 1; 1; 1; 2; 3; 4]
;; run_test "replicate: checks to see if it works with non-empty list" test

(* snoc v l returns l but with v appended onto the end (reverse cons) *)
let rec snoc (v:'a) (l:'a list) : 'a list =
  match l with
  |[] -> v :: []
  |x :: xs -> x :: snoc v xs 

let test () : bool = 
  snoc 4 [1;2;3] = [1; 2; 3; 4]
;; run_test "snoc: adding an integer to the end of an int list" test

let test () : bool =
  snoc "giani" ["hi";"my";"name";"is"] = ["hi";"my";"name";"is";"giani"]
;; run_test "snoc: testing a string list" test

let test () : bool =
  snoc "hi" [] = ["hi"]
;; run_test "snoc: adding to an empty list " test

(* zip l1 l2 returns a list containing pairs of elements from l1 l2.
 * For example, zip [1; 2; 3] [4; 5; 6] = [(1, 2); (3, 4); (5, 6)].
 * If either list if longer than the other, the remaining elements are
 * dropped from the result. *)
let rec zip (l1:'a list) (l2:'b list) :('a * 'b)  list =
  let rec size (l : 'a list) : int =
    match l with
    |[] -> 0
    |x :: xs -> 1 + size xs
  in
  if size l1 > size l2
  then
  match l1 with
  |[] -> []
  |x :: xs -> match l2 with
    |[] -> []
    |y :: ys -> (x, y) :: zip xs ys
  else
    match l2 with
    |[] -> []
    |x :: xs -> match l1 with
      |[] -> []
      |y :: ys -> (y, x) :: zip ys xs

let test () : bool =
  zip [1;2] [3;4] = [(1, 3); (2, 4)]
;; run_test "zip: takes two lists [1;2] and [3;4] to produce a list of pairs [(1,3); (2,4)] " test

let test () : bool =
  zip [1; 2; 3] [1; 2] = [(1,1); (2,2)]
;; run_test "zip: testing lists of different sizes" test

let test () : bool =
  zip ["hola"; "mi"; "nombre"] ["hi"; "my"; "name"] = [("hola","hi"); ("mi","my"); ("nombre","name")]
    ;; run_test "zip: testing lists of strings" test

(* rev l returns l but reversed. *)
 let rec rev (l:'a list) : 'a list =
   let rec aux (pre : 'a list)(l2 : 'a list) : 'a list =
     match l2 with
     |[] -> pre
     |x :: xs ->  aux (x :: pre) xs
   in
   aux [] l

let test () : bool =
  rev [1;2;3] = [3;2;1]
;; run_test "rev: test should reverse [1;2;3] into [3;2;1]" test

let test () : bool =
  rev ["giani"; "is"; "me"] = ["me"; "is"; "giani"]
;; run_test "rev: testing the rev of a list of strings" test

let test () : bool =
  rev [] = []
    ;; run_test "rev: an empty list should return an empty list" test

let rec contains (v:'a) (l:'a list) : bool =
  match l with
  |x :: xs -> if v == x then true
  else contains v xs
  |[] -> false
        
(* dedup l returns a copy of l but with all duplicates removed *)
let rec dedup (l:'a list) : 'a list =
  let rec contains (e:'a) (l:'a list) : bool =
    match l with
    |x :: xs -> if e == x then true
    else contains e xs
    |[] -> false
  in
  match l with
  |[] -> l
  |x :: xs -> if contains x xs == true
  then dedup xs
  else x :: dedup xs
    
let test () : bool =
  dedup [1;1;2;2;3;3] = [1;2;3]
;; run_test "dedup: shold remove all duplicates" test

let test () : bool =
  dedup [1;2;3] = [1;2;3]
;; run_test "dedup: should remove nothing" test

let test () : bool =
  dedup [1;2;3;1;5;9;5;2;2;4] = [3;1;9;5;2;4]
;; run_test "dedup:tests duplicates that are not next to one another in a list" test

(* concat ll returns a list containing all of the lists of ll concatenated
 * together.  For example, concat [ [1; 2]; [3; 4]; [5; 6] ] =
 * [1; 2; 3; 4; 5; 6].
 *)
let rec concat (ll:('a list) list) : 'a list =
  match ll with 
  |[] -> []
  |y :: ys ->  y @  concat ys 

let test () : bool =
  concat [[1;2];[3;4];[5;6]] = [1;2;3;4;5;6]
;; run_test "concat: tests the initial example of concat [[1;2];[3;4];[5;6]] to get [1;2;3;4;5;6] " test

let test () : bool =
  concat [[1;2;3;4;5;6]; [1;2]] = [1;2;3;4;5;6;1;2]
;; run_test "concat: testing lists with different sized lists" test

let test () : bool =
  concat [["a";"b";"c"];["e";"f";"g"]] = ["a";"b";"c";"e";"f";"g"]
;; run_test "concat: testing concat with a list of list of strings" test

(* split i l returns a pair of lists constructed by splitting l at index
 * i in the list.  The ith element is included in the second list.  For
 * example, split 2 [1; 2; 3; 4; 5] = ([1; 2], [3; 4; 5]). *)
let rec split (i:int) (l:'a list) : ('a list * 'a list) = 
  let rec first (j:int) (l2: 'a list) : 'a list =
    if j > 0 then
      match l2 with
      |[] -> []
      |x :: xs -> x :: first (j - 1) xs
    else
      []
  in
  let rec second (k:int) (l3: 'a list) : 'a list =
    if k > 0 then
      match l3 with
      |[] -> []
      |x :: xs -> second (k - 1) xs
    else
      l3
  in
  let x = first i l in
  let y = second i l
  in
  (x, y)

let test () : bool =
  split 2 [1;2;3;4;5] = ([1;2],[3;4;5])
;; run_test "split: tests initial example of split 2 [1; 2; 3; 4; 5] = ([1; 2], [3; 4; 5]) " test

let test () : bool =
  split 0 [1;2;3;4;5] = ([],[1;2;3;4;5])
;; run_test "split: testing the split of a list with 0" test

let test () : bool =
  split 5 [1;2;3;4;5] = ([1;2;3;4;5], [])
;; run_test "split: testing the split of a list with it's full size" test

(* prefixes l returns a list of all the prefixes of l (as lists).  For
 * example, prefixes [1; 2; 3; 4; 5] = [[]; [1]; [1;2]; [1;2;3]; [1;2;3;4];
 * [1;2;3;4;5]] *)
let rec prefixes (l:'a list) : ('a list) list =
  let rec newprefix (l1 : 'a list) (pre : 'a list)(n : int) : ('a list) list =
      match l1 with
      |[] -> [(rev pre)]
      |x :: xs -> (rev pre) :: newprefix (x::l1) xs (n-1)
  in   
  let rec size (l2 : 'a list) : int =
    match l2 with
    |[] -> 0
    |x :: xs -> 1 + size xs
  in
  newprefix l [] (size l)
    
let test () : bool =
  prefixes [1; 2; 3; 4; 5] = [[]; [1]; [1;2]; [1;2;3]; [1;2;3;4];[1;2;3;4;5]]
;; run_test "prefixes: initial example from PM" test

let test () : bool =
  prefixes ["hi";"hello"; "hey"] = [[];["hi"];["hi";"hello"];["hi";"hello"; "hey"]]  
;; run_test "prefixes: testing with strings " test

let test () : bool =
  prefixes [] = [[]]
    ;; run_test "prefixes: testing prefizes of empty list" test
