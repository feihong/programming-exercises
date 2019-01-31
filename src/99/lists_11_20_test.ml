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
