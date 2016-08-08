(* 16 *)
type seasoning =
    Salt
  | Pepper

(* 21 *)
type num =
    Zero
  | One_more_than of num

(* 25 *)
One_more_than (One_more_than Zero)

(* 32 *)
type 'a open_faced_sandwich =
    Bread of 'a
  | Slice of 'a open_faced_sandwich

(* 47 *)
Bread (Bread (One_more_than Zero))
