open Prelude

(* 11 *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode lst =
  let make_rle current x = match current with
  | 0 -> One x
  | current -> Many (current + 1, x)
  in
  let rec aux current acc = function
  | [] -> acc
  | [x] -> make_rle current x :: acc
  | a :: (b :: _ as xs) ->
    if a = b then
      aux (current + 1) acc xs
    else
      aux 0 (make_rle current a :: acc) xs
  in aux 0 [] lst |. List.reverse

(* 12 *)
let decode lst =
  lst
  |. List.map (function One x -> [x] | Many (n, x) -> List.make n x)
  |. List.flatten

(* 13 *)
(* Same as my solution for 11 *)

(* 14 *)
let duplicate lst =
  let rec aux acc = function
  | [] -> acc
  | x :: xs -> aux (x :: x :: acc) xs
  in aux [] lst |. List.reverse

(* 15 *)
let replicate lst n =
  let rec aux acc = function
  | [] -> acc
  | x :: xs ->
    let acc' = (List.make n x) @ acc in
    aux acc' xs
  in aux [] lst |. List.reverse

(* 16 *)
let drop lst n =
  let rec aux count acc = function
  | [] -> acc
  | x :: xs ->
    let acc' = if count mod n = 0 then acc else x :: acc in
    aux (count + 1) acc' xs
  in aux 1 [] lst |. List.reverse

(* 17 *)
let split lst n =
  let rec aux count acc = function
  | [] -> (List.reverse acc , [])
  | h :: t as xs ->
    if count <= 0
    then (List.reverse acc, xs)
    else aux (count - 1) (h :: acc) t
  in aux n [] lst

(* 18 *)
let slice lst i k =
  let rec drop n = function
  | [] -> []
  | _ :: t as xs ->
    if n <= 0 then xs
    else drop (n - 1) t
  in
  let rec take n acc = function
  | [] -> acc
  | x :: xs ->
    if n <= 0 then acc
    else take (n - 1) (x :: acc) xs
  in lst |> drop i |> take (k - i + 1) [] |> List.reverse

(* 19 *)
let rotate lst = function
  | 0 -> lst
  | n ->
    let n = if n < 0 then (List.length lst) + n else n in
    let (a, b) = split lst n in b @ a

(* Tests *)
let () = describe "Lists" @@ fun () ->

test "encode" (fun () ->
  expect_all [
    encode [], [];
    encode [1], [One 1];
    encode [33;33], [Many (2, 33)];
    encode [33;33;33;22], [Many (3, 33); One 22];
    encode [44; 55], [One 44; One 55];
  ]);

test "encode string" (fun () ->
   encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
   ===
   [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]);

test "decode" (fun () ->
  expect_all [
    decode [], [];
    decode [One 1], [1];
    decode [Many (3, 1)], [1;1;1];
  ]);

test "decode string" (fun () ->
  decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]
  ===
  ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
);

test "duplicate" (fun () ->
  expect_all [
    duplicate [], [];
    duplicate [1], [1;1];
    duplicate [1; 2], [1;1;2;2]
  ]);

test "duplicate string" (fun () ->
  duplicate ["a";"b";"c";"c";"d"]
  ===
  ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
);

test "replicate string" (fun () ->
  replicate ["a";"b";"c"] 3
  ===
  ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
);

test "drop" (fun () ->
  expect_all [
    drop [] 10, [];
    drop [1] 1, [];
    drop [1] 2, [1];
    drop [1;2] 2, [1];
    drop [1;2;3] 1, [];
  ]);

test "drop string" (fun () ->
  drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3
  ===
  ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
);

test "split" (fun () ->
  expect_all [
    split [] 10, ([], []);
    split [1;2;3] 0, ([], [1;2;3]);
    split [1;2;3] 5, ([1;2;3], []);
    split [1;2;3;4;5] 3, ([1;2;3], [4;5]);
  ]);

test "split string" (fun () ->
  split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3
  ===
  (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
);

test "slice" (fun () ->
  expect_all [
    slice [1;2;3] 0 0, [1];
    slice [1;2;3;4] 1 2, [2;3];
    slice [1;2;3] 3 5, [];
    slice [1;2;3;4] 3 10, [4];
    slice [1;2;3;4] 0 10, [1;2;3;4];
  ]);

test "slice string" (fun () ->
  slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6
  ===
  ["c"; "d"; "e"; "f"; "g"]
);

test "rotate" (fun () ->
  expect_all [
    (
      rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3,
      ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
    );
    (
      rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2),
      ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
    )
  ]);
