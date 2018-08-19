(* 4 *)
type 'a list =
    Empty
  | Cons of 'a * 'a list

(* 5 *)
type box =
    Bacon
  | Ix of int

(* 9 *)
let is_bacon =
  function
    Bacon -> true
  | Ix n -> false

let rec where_is =
  function
    Empty -> 0
  | Cons (a_box, rest) ->
      if is_bacon a_box
      then 1
      else 1 + where_is rest

(* 14 *)
exception No_bacon of int

let rec where_is =
  function
    Empty -> raise (No_bacon 0)
  | Cons (a_box, rest) ->
      if is_bacon a_box
      then 1
      else 1 + where_is rest

(* 31 *) ;; 
(* The ;; above is required as the expression below isn't bound to
 * anything, so the compiler thinks it's part of the let above,
 * which is nonsense and leads to a syntax error. *)
try (where_is (Cons (Ix 5, Cons (Ix 13, Cons (Ix 8, Empty))))) with
  No_bacon an_int -> an_int

(* 66 *)
let eq_int ((n : int), (m : int)) = (n = m)

exception Out_of_range

let rec list_item =
  function
    n, Empty -> raise Out_of_range
  | n, Cons (a_box, rest) ->
      if eq_int (n, 1)
      then a_box
      else list_item (n - 1, rest)

(* 67 *)
let rec find (n, boxes) =
  try check (n, boxes, list_item (n, boxes)) with
    Out_of_range -> find (n / 2, boxes)
and check =
  function
    n, boxes, Bacon -> n
  | n, boxes, Ix i -> find (i, boxes)

(* 75 *)
let t = Cons (Ix 5, Cons (Ix 4, Cons (Bacon,
  Cons (Ix 2, Cons (Ix 3, Empty)))))

(* 98 *)
let rec path (n, boxes) =
  Cons (n, (try check (boxes, list_item (n, boxes)) with
              Out_of_range -> path (n / 2, boxes)))
and check =
  function
    boxes, Bacon -> Empty
  | boxes, Ix i -> path (i, boxes)
