/* 1 */
type shish_kebab =
  | Skewer
  | Onion(shish_kebab)
  | Lamb(shish_kebab)
  | Tomato(shish_kebab);

/* 15 */
let rec only_onions =
  fun
  | Skewer => true
  | Onion(x) => only_onions(x)
  | Lamb(x) => false
  | Tomato(x) => false;

/* 63 */
let rec is_vegetarian =
  fun
  | Skewer => true
  | Onion(x) => is_vegetarian(x)
  | Lamb(x) => false
  | Tomato(x) => is_vegetarian(x);

/* 64 */
type shish('a) =
  | Bottom('a)
  | Onion(shish('a))
  | Lamb(shish('a))
  | Tomato(shish('a));

/* 67 */
type rod =
  | Dagger
  | Fork
  | Sword;

/* 68 */
type plate =
  | Gold_plate
  | Silver_plate
  | Brass_plate;

/* 73 */
let rec is_veggie =
  fun
  | Bottom(x) => true
  | Onion(x) => is_veggie(x)
  | Lamb(x) => false
  | Tomato(x) => is_veggie(x);

/* 108 */
let rec what_bottom =
  fun
  | Bottom(x) => x
  | Onion(x) => what_bottom(x)
  | Lamb(x) => what_bottom(x)
  | Tomato(x) => what_bottom(x);
/* Shorter version: */
let rec what_bottom =
  fun
  | Bottom(x) => x
  | Onion(x)
  | Lamb(x)
  | Tomato(x) => what_bottom(x);
