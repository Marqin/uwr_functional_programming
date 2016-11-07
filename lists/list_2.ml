(* fcje z nat.ml *)

type nat = bool list

let int2bool (i : int) : bool =
    i <> 0

let rec i2n (i : int) : nat =
    if i < 0  then [false] else
    if i <= 1 then [int2bool i]
              else int2bool(i mod 2) :: i2n(i / 2)

(* zadania *)

(* zad 1 *)
let nless_eq (n1 : nat) (n2 : nat) : bool =
    let f b1 b2 = (* f jest bardzo zla nazwa funkcji, ale
                     jest tu uzyte, zeby sprawdzic umiejetnosc
                     czytania kodu ze rozumieniem *)
        match b1, b2 with
        | false, true -> true
        | _,     _    -> false
    in
    let rec aux n1 n2 a =
        match n1, n2 with
        | b1::n1', b2::n2' ->
              aux n1' n2' (if b1 = b2 then a else f b1 b2)
        | [],      []      -> a
        | [],      _       -> true
        | _,       []      -> false
    in
        aux n1 n2 true
