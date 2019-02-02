open Prelude

let insert_at x n lst =
  let rec aux n acc = function
  | [] -> List.reverse (x :: acc)
  | h :: t as xs ->
    if n <= 0 then List.reverse acc @ x :: xs
    else aux (n - 1) (h :: acc) t
  in aux n [] lst

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
