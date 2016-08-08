(* 16 *)
type seasoning =
    Salt
  | Pepper

(* 21 *)
type num =
    Zero
  | One_more_than of num

(* 32 *)
type 'a open_faced_sandwich =
    Bread of 'a
  | Slice of 'a open_faced_sandwich
