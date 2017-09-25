let eq_int ((n : int), (m : int)) = n = m

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
  else succ (plus (pred n, m))

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
  else succ (plus (pred n, m))

(* 18 *)
module type N = sig
  type number
  exception Too_small
  val succ : number -> number
  val pred : number -> number
  val is_zero : number -> bool
end

(* 26 *)
module NumberAsNum0 ( ) : N = struct
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

module NumberAsInt0 ( ) : N = struct
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
module IntStruct0 = NumberAsInt0( )

(* 35 *)
module NumStruct0 = NumberAsNum0( )

(* 40 *)
module type P = sig
  type number
  val plus : number * number -> number
end

(* 41 *)
module PON (Na : N) : P = struct
  type number = Na.number
  let rec plus (n, m) =
    if Na.is_zero m
    then m
    else Na.succ (plus (Na.pred n, m))
end

(* 47 *)
module IntArith0 = PON(IntStruct0)

(* 62 *)
module type N_C_R = sig
  type number
  exception Too_small
  val conceal : int -> number
  val reveal : number -> int
  val is_zero : number -> bool
  val succ : number -> number
  val pred : number -> number
end

(* 65 *)
module NumberAsInt ( ) : N_C_R = struct
  type number = int
  exception Too_small
  let conceal n = n
  let reveal n = n
  let is_zero n = eq_int (n, 0)
  let succ n = n + 1
  let pred n =
    if is_zero n then raise Too_small else n - 1
end

module NumberAsNum ( ) : N_C_R = struct
  type number = Zero | One_more_than of number
  exception Too_small
  let rec conceal n =
    if eq_int (n, 0)
    then Zero
    else One_more_than (conceal (n - 1))
  let rec reveal = function
    | Zero -> 0
    | One_more_than n -> 1 + (reveal n)
  let is_zero = function Zero -> true | _ -> false
  let succ n = One_more_than n
  let pred = function
    | Zero -> raise Too_small
    | One_more_than n -> n
end

(* 66 *)
module IntStruct = NumberAsInt( )

module IntArith = PON(IntStruct)

module NumStruct = NumberAsNum( )

module NumArith = PON(NumStruct)

(* 93 *)
module NumberAsInt2 ( )
    : N with type number = int
  = struct
    type number = int
    exception Too_small
    let is_zero n = eq_int (n, 0)
    let succ n = n + 1
    let pred n =
      if is_zero n then raise Too_small else n - 1
  end

(* 95 *)
module IntStruct2 = NumberAsInt2( )

(* 96 *)
module IntArith2 = PON(IntStruct2)

(* 114 *)
module type S = sig
  type number1
  type number2
  val similar : number1 * number2 -> bool
end

(* 115 *)
module Same (Na : N) (Nb: N)
    : S with type number1 = Na.number
        with type number2 = Nb.number
  = struct
    type number1 = Na.number
    type number2 = Nb.number
    let rec sim (n, m) =
      if Na.is_zero n
      then Nb.is_zero m
      else sim (Na.pred n, Nb.pred m)
    let similar (n, m) =
      try sim (n, m) with
      | Na.Too_small -> false
      | Nb.Too_small -> false
  end

(* 118 *)
module SimIntNum = Same(IntStruct)(NumStruct)

(* 119 *)
module SimNumInt = Same(NumStruct)(IntStruct)

(* 127 *)
(* Can't reproduce in OCaml *)

(* 129 *)
module type J = sig
  val new_plus : int * int -> int
end

module NP (Na : N_C_R with type number = int)
          (Pa : P with type number = int)
    : J = struct
  let new_plus (x, y) =
    Na.reveal
      (Pa.plus
        ( Na.conceal x
        , Na.conceal y ))
end

(* Can't reproduce *)
module NPStruct = NP(struct
  type number = int
  exception Too_small
  let conceal n = n
  let reveal n = n
  let is_zero n = eq_int (n, 0)
  let succ n = n + 1
  let pred n =
    if is_zero n then raise Too_small else n - 1
end)(struct
  type number = int
  let rec plus (x, y) =
    x + y
end)

(* 147 *)
module type T = sig
  type number
  val times : number * number -> number
end

(*
module TON (Na : N) (Pa : P)
    : T with type number = Na.number
  = struct
    type number = Na.number
    let rec times (n, m) =
      if Na.is_zero m
      then m
      else Pa.plus (n, times (n, Na.pred m))
  end
*)
