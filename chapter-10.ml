let eq_int ((n : int), (m : int)) = (n = m)

(* 4 *)
exception Too_small

let is_zero n = eq_int (n, 0)

let pred n =
  if is_zero n
  then raise Too_small
  else n - 1

let succ n = n + 1

let rec plus (n, m) =
  if is_zero n
  then m
  else succ (plus (pred n), m)

(* 5 *)
type num =
    Zero
  | One_more_than of num

let is_zero =
  function
    Zero -> true
  | not_zero -> false

let pred =
  function
    Zero -> raise Too_small
  | One_more_than n -> n

let succ n = One_more_than n

let rec plus (n, m) =
  if is_zero n
  then m
  else succ (plus (pred n), m)

(* 18 *)
module type N = sig
  type number
  exception Too_small
  val succ : number -> number
  val pred : number -> number
  val is_zero : number -> bool
end

(* 26 *)
module NumberAsNum () : N = struct
  type num =
      Zero
    | One_more_than of num
  type number = num
  exception Too_small
  let succ n =
    One_more_than n
  let pred =
    function
      Zero -> raise Too_small
    | One_more_than n -> n
  let is_zero =
    function
      Zero -> true
    | a_num -> false
end

module NumberAsInt () : N = struct
  type number = int
  exception Too_small
  let succ n = n + 1
  let pred n =
    if eq_int (n, 0)
    then raise Too_small
    else n - 1
  let is_zero n =
    eq_int (n, 0)
end

(* 33 *)
module IntStruct = NumberAsInt ()

(* 35 *)
module NumStruct = NumberAsNum ()

(* 40 *)
module type P = sig
  type number
  val plus : number * number -> number
end

(* 41 *)
