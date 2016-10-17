type 'a flbt = int -> 'a

let log2 x = log x /. log 2.
let get_bit x = truncate (floor (log2 (float x)))
let get_right_bit x = get_bit x - 1
let get_left_bit x = get_bit x + 1

exception Not_in_tree

let test_tree n =
    match n with
      1 -> 'r'
    | 2 -> 'a'
    | 3 -> 'b'
    | 6 -> 'c'
    | _ -> raise Not_in_tree

(* zad 2 *)
let check x = (x lsr (get_right_bit x)) land 1
let zad2 n =
  if n < 1 then raise Not_in_tree else
    if check n == 0 then 'a' else 'b'

(* zad 3 *)
let pos_root = 1
let pos_prepare x = x lor (truncate (2. ** float (get_left_bit x)))
let pos_left_child x = (pos_prepare x) land (lnot (truncate (2. ** float (get_bit x))))
let pos_right_child x = pos_prepare x

(* zad 4 *)
let is_empty x = try (x 1; false) with Not_in_tree -> true
let get t n = try (t n) with Not_in_tree -> raise Not_in_tree

(* zad 5 *)
let empty n = raise Not_in_tree

let join s t v n =
  if n < 1 then raise Not_in_tree else
    if n == 1 then v else
      if (n mod 2) == 0 then s n else t n

let left_subtree t n =
  if n < 1 then raise Not_in_tree else t (n lsl 1)

let right_subtree t n =
  if n < 1 then raise Not_in_tree else t ((n lsl 1) lor 1)

(* zad 6 *)
let rec depth t =
  if is_empty t then 0 else
    1 + max (depth (left_subtree t)) (depth (right_subtree t))
