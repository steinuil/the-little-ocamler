/* 1 */
type pizza('a) =
  | Bottom
  | Topping(('a, pizza('a)));

/* 3 */
type fish =
  | Anchovy
  | Lox
  | Tuna;

/* 11 */
let rec rem_anchovy =
  fun
  | Bottom => Bottom
  | [@implicit_arity] Topping(Anchovy, p) => rem_anchovy(p)
  | [@implicit_arity] Topping(Tuna, p) =>
    [@implicit_arity] Topping(Tuna, rem_anchovy(p))
  | [@implicit_arity] Topping(Lox, p) =>
    [@implicit_arity] Topping(Lox, rem_anchovy(p));

/* 13 */
let rec rem_anchovy =
  fun
  | Bottom => Bottom
  | [@implicit_arity] Topping(Anchovy, p) => rem_anchovy(p)
  | [@implicit_arity] Topping(t, p) =>
    [@implicit_arity] Topping(t, rem_anchovy(p));

/* 14 */
let rec rem_tuna =
  fun
  | Bottom => Bottom
  | [@implicit_arity] Topping(Anchovy, p) =>
    [@implicit_arity] Topping(Anchovy, rem_tuna(p))
  | [@implicit_arity] Topping(Tuna, p) => rem_tuna(p)
  | [@implicit_arity] Topping(Lox, p) =>
    [@implicit_arity] Topping(Lox, rem_tuna(p));

/* 17 */
type fish =
  | Tuna
  | Lox
  | Anchovy;

/* 21 */
let rec rem_tuna =
  fun
  | Bottom => Bottom
  | [@implicit_arity] Topping(Tuna, p) => rem_tuna(p)
  | [@implicit_arity] Topping(t, p) =>
    [@implicit_arity] Topping(t, rem_tuna(p));

/* 38 */
let rec eq_fish =
  fun
  | (Anchovy, Anchovy) => true
  | (Lox, Lox) => true
  | (Tuna, Tuna) => true
  | (a_fish, another_fish) => false;

/* 40 */
let rec rem_fish =
  fun
  | (x, Bottom) => Bottom
  | (x, [@implicit_arity] Topping(t, p)) =>
    if (eq_fish((t, x))) {
      rem_fish((x, p));
    } else {
      [@implicit_arity] Topping(t, rem_fish((x, p)));
    };

/* 57 */
let eq_int = ((n: int, m: int)) => n == m;
/* This also works*/
let eq_int: ((int, int)) => bool = ((n, m)) => n == m;

let rec rem_int =
  fun
  | (x, Bottom) => Bottom
  | (x, [@implicit_arity] Topping(t, p)) =>
    if (eq_int((t, x))) {
      rem_int((x, p));
    } else {
      [@implicit_arity] Topping(t, rem_int((x, p)));
    };

/* 66 */
let rec subst_fish =
  fun
  | (n, a, Bottom) => Bottom
  | (n, a, [@implicit_arity] Topping(t, p)) =>
    if (eq_fish((t, a))) {
      [@implicit_arity] Topping(n, subst_fish((n, a, p)));
    } else {
      [@implicit_arity] Topping(t, subst_fish((n, a, p)));
    };

let rec subst_int =
  fun
  | (n, a, Bottom) => Bottom
  | (n, a, [@implicit_arity] Topping(t, p)) =>
    if (eq_int((t, a))) {
      [@implicit_arity] Topping(n, subst_int((n, a, p)));
    } else {
      [@implicit_arity] Topping(t, subst_int((n, a, p)));
    };

/* 71 */
let rec eq_num =
  fun
  | (Zero, Zero) => true
  | (One_more_than(n), One_more_than(m)) => eq_num((n, m))
  | (n, m) => false;
