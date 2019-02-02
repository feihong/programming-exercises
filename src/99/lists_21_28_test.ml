open Prelude

(* 21 *)
let insert_at x n lst =
  let rec aux n acc = function
  | [] -> List.reverse (x :: acc)
  | h :: t as xs ->
    if n <= 0 then List.reverse acc @ x :: xs
    else aux (n - 1) (h :: acc) t
  in aux n [] lst

(* 22 *)
let range i k =
  let fn = if i <= k then (-) else (+) in
  let rec aux acc i k = match k with
  | k when i = k -> k :: acc
  | k -> aux (k :: acc) i (fn k 1)
  in aux [] i k

(* 23 *)
let rand_select lst n =
  let rec remove i acc = function
  | [] -> raise Not_found
  | h :: t ->
    if i = 0 then (h, acc @ t)
    else remove (i - 1) (h :: acc) t
  in
  let rec aux acc_len acc lst_len = function
  | [] -> acc
  | _ :: _ as lst ->
    if acc_len >= n then acc
    else
      let (x, lst') = remove (Random.int lst_len) [] lst in
      aux (acc_len + 1) (x :: acc) (lst_len - 1) lst'
  in aux 0 [] (List.length lst) lst

(* 24 *)
let lotto_select n m =
  range 1 m |. rand_select n

(* Tests *)
let () = describe "Lists" @@ fun () ->

test "insert_at" (fun () ->
  expect_all [
    (
      insert_at "alfa" 1 ["a";"b";"c";"d"],
      ["a"; "alfa"; "b"; "c"; "d"]
    );
    (
      insert_at "alfa" 3 ["a";"b";"c";"d"],
      ["a"; "b"; "c"; "alfa"; "d"]
    );
    (
      insert_at "alfa" 4 ["a";"b";"c";"d"],
      ["a"; "b"; "c"; "d"; "alfa"]
    );
    (
      insert_at "alfa" 10 ["a";"b";"c";"d"],
      ["a"; "b"; "c"; "d"; "alfa"]
    )
  ]);

test "range" (fun () ->
  expect_all [
    range 1 5, [1;2;3;4;5];
    range 6 6, [6];
    range 9 6, [9;8;7;6;];
    range (-2) 4, [-2;-1;0;1;2;3;4];
    range 5 (-3), [5;4;3;2;1;0;-1;-2;-3];
  ]);

test "rand_select" (fun () ->
  expect_all [
    rand_select [1] 0, [];
    rand_select [] 5, [];
    rand_select [1;2;3] 1, [1];
    rand_select [1;2;3] 2, [1;2];
    rand_select [1;2;3] 3, [2;1;3];
    rand_select [1;2;3] 3, [1;2;3];
    rand_select [1;2;3;4;5;6;7] 3, [2;1;5];
    rand_select [1;2;3;4;5;6;7] 3, [4;5;6];
    rand_select [1;2;3;4;5;6;7] 3, [3;1;5];
    rand_select [1;2;3;4;5;6;7] 3, [2;7;3];
    rand_select [1;2;3;4;5;6;7] 3, [6;1;2];
  ]);

test "lotto_select" (fun () ->
  expect_all [
    lotto_select 1 2, [2];
    lotto_select 1 2, [1];
    lotto_select 3 10, [5;8;7];
    lotto_select 3 10, [7;6;1];
  ]);
