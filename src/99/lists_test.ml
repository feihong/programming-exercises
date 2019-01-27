open Belt
open Jest

let (===) a b = let open Expect in expect a |> toEqual b

let expect_all xs =
  let (a, b) = xs |> List.toArray |> Array.unzip in
  let open Expect in expect a |> toEqual b

(* 1 *)
let rec last = function [] -> None | [x] -> Some x | _x :: xs -> last xs

(* 2 *)
let rec last_two = function
| [] | [_] -> None
| [a; b] -> Some (a, b)
| _ :: xs -> last_two xs

(* 3 *)
let rec at n = function
| [] -> None
| x :: xs ->
  match n with
  | 1 -> Some x
  | n when n < 1 -> None
  | n -> at (n - 1) xs

(* 4 *)
let length xs =
  let rec aux acc = function
  | [] -> acc
  | _ :: xs -> aux (acc + 1) xs
  in aux 0 xs

(* 5 *)
let reverse xs =
  let rec aux acc = function
  | [] -> acc
  | x :: xs -> aux (x :: acc) xs
  in aux [] xs

(* 6 *)
let is_palindrome xs = xs = reverse xs

(* 7 *)
(* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten list =
    let rec aux acc = function
    | [] -> acc
    | One x :: xs -> aux (x :: acc) xs
    | Many x :: xs ->
      let acc' = aux acc x in
      aux acc' xs
    in List.reverse (aux [] list)

(* Tests *)
let () = describe "Lists" @@ fun () ->

test "last" (fun () ->
  expect_all [
    last [], None;
    last [1; 2; 3], Some 3;
  ]);

test "last_two" (fun () ->
  expect_all [
    last_two [], None;
    last_two [1], None;
    last_two [1; 2], Some (1, 2);
    last_two [1; 2; 3], Some (2, 3);
  ]);

test "at" (fun () ->
  expect_all [
    at 1 [], None;
    at 1 [1], Some 1;
    at 0 [1;2;3], None;
    at 10 [1;2;3], None;
    at 4 [1;2;3;4;5;6], Some 4;
  ]);

test "length" (fun () ->
  expect_all [
    length [], 0;
    length [1;2;3], 3;
  ]);

test "reverse" (fun () ->
  expect_all [
    reverse [], [];
    reverse [1], [1];
    reverse [1;2;3], [3;2;1];
  ]);

test "is_palindrome" (fun () ->
  expect_all [
    is_palindrome [], true;
    is_palindrome [1], true;
    is_palindrome [1;2], false;
    is_palindrome [1;2;3;2;1], true;
  ]);

test "flatten" (fun () ->
  expect_all [
    flatten [One 1], [1];
    flatten [Many [One 1; One 2; One 3]], [1;2;3];
    flatten [Many [One 1; One 2]; One 3; Many []; One 4], [1;2;3;4];
  ]);

test "flatten strings" (fun () ->
  flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]
  ===
  ["a"; "b"; "c"; "d"; "e"]
)
