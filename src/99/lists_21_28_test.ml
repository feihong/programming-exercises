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

(* 25 *)
let permutation lst =
  rand_select lst (List.length lst)

(* 26 *)
let split lst n =
  let rec aux count acc = function
  | [] -> (List.reverse acc , [])
  | h :: t as lst ->
    if count <= 0
    then (List.reverse acc, lst)
    else aux (count - 1) (h :: acc) t
  in aux n [] lst

let extract k lst =
  let rec aux acc = function
  | [] -> []
  | _ :: t as lst ->
    if List.length lst = k
    then [lst] :: acc
    else
      let (front, back) = split lst (k - 1) in
      let acc' = List.map back (fun x -> x :: front) :: acc in
      aux acc' t
  in
    if k <= 1
    then List.map lst (fun x -> [x])
    else
      aux [] lst
      |. List.flatten
      (* Sort for presentation purposes *)
      |. List.map (fun l -> List.sort l compare)
      |. List.sort (fun a b ->
          match (List.head a, List.head b) with
          | (Some a, Some b) -> compare a b
          | _ -> 0)

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

test "permutation" (fun () ->
  expect_all [
    permutation [], [];
    permutation [1;2;3], [2;3;1];
    permutation [1;2;3], [1;3;2];
    permutation [1;2;3], [2;1;3];
  ]);

test "extract" (fun () ->
  expect_all [
    extract 2 [], [];
    extract 1 [1;2;3;4], [[1];[2];[3];[4]];
    extract 2 [1;2;3], [[1;2];[1;3];[2;3]];
    extract 3 [1;2;3;4;5], [[1;2;3];[1;2;4];[1;2;5];[2;3;4];[2;3;5];[3;4;5]];
  ]);

test "extract string" (fun () ->
  extract 2 ["a";"b";"c";"d"]
  ===
  [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
);
