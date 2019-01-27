open Belt
open Jest

let expect_all xs =
  let (a, b) = xs |> List.toArray |> Array.unzip in
  let open Expect in
  expect a |> toEqual b

let rec last = function [] -> None | [x] -> Some x | _x :: xs -> last xs

let rec last_two = function
| [] | [_] -> None
| [a; b] -> Some (a, b)
| _ :: xs -> last_two xs


(* let at n xs =
  let rec go acc xs = match (acc, xs) with
  | (_, []) -> None
  | (1, x :: _) -> Some x
  | (acc, _ :: xs) -> go (acc - 1) xs
  in
  if n >= 1 then go n xs else None *)

let rec at n = function
| [] -> None
| x :: xs ->
  match n with
  | 1 -> Some x
  | n when n < 1 -> None
  | n -> at (n - 1) xs

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
