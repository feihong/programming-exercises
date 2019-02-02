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
