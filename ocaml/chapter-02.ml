(* 1 *)
type shish_kebab =
    Skewer
  | Onion of shish_kebab
  | Lamb of shish_kebab
  | Tomato of shish_kebab

(* 15 *)
let rec only_onions =
  function
    Skewer -> true
  | Onion x -> only_onions x
  | Lamb x -> false
  | Tomato x -> false

(* 63 *)
let rec is_vegetarian =
  function
    Skewer -> true
  | Onion x -> is_vegetarian x
  | Lamb x -> false
  | Tomato x -> is_vegetarian x

(* 64 *)
type 'a shish =
    Bottom of 'a
  | Onion of 'a shish
  | Lamb of 'a shish
  | Tomato of 'a shish

(* 67 *)
type rod = Dagger | Fork | Sword

(* 68 *)
type plate = Gold_plate | Silver_plate | Brass_plate

(* 73 *)
let rec is_veggie =
  function
    Bottom x -> true
  | Onion x -> is_veggie x
  | Lamb x -> false
  | Tomato x -> is_veggie x

(* 108 *)
let rec what_bottom =
  function
    Bottom x -> x
  | Onion x -> what_bottom x
  | Lamb x -> what_bottom x
  | Tomato x -> what_bottom x
(* Shorter version: *)
let rec what_bottom =
  function
    Bottom x -> x
  | Onion x | Lamb x | Tomato x -> what_bottom x
