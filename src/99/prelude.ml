include Jest

let (===) a b = let open Expect in expect a |> toEqual b

let expect_all xs =
  let (a, b) = xs |> List.toArray |> Array.unzip in
  let open Expect in expect a |> toEqual b
