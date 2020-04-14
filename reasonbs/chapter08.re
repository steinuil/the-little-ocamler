/* 1 */
type list('a) =
  | Empty
  | Cons('a, list('a));
let eq_int = ((n: int, m: int)) => n == m;
let l =
  [@implicit_arity]
  Cons(
    15,
    [@implicit_arity]
    Cons(
      6,
      [@implicit_arity]
      Cons(
        15,
        [@implicit_arity]
        Cons(
          17,
          [@implicit_arity] Cons(15, [@implicit_arity] Cons(8, Empty)),
        ),
      ),
    ),
  );

/* 3 */
type orapl =
  | Orange
  | Apple;

let eq_orapl =
  fun
  | (Orange, Orange) => true
  | (Apple, Apple) => true
  | (one, another) => false;

/* 4 */
let rec subst_int =
  fun
  | (n, a, Empty) => Empty
  | (n, a, [@implicit_arity] Cons(e, t)) =>
    if (eq_int((a, e))) {
      [@implicit_arity] Cons(n, subst_int((n, a, t)));
    } else {
      [@implicit_arity] Cons(e, subst_int((n, a, t)));
    };

let rec subst_orapl =
  fun
  | (n, a, Empty) => Empty
  | (n, a, [@implicit_arity] Cons(e, t)) =>
    if (eq_orapl((a, e))) {
      [@implicit_arity] Cons(n, subst_orapl((n, a, t)));
    } else {
      [@implicit_arity] Cons(e, subst_orapl((n, a, t)));
    };

/* 9 */
let rec subst =
  fun
  | (rel, n, a, Empty) => Empty
  | (rel, n, a, [@implicit_arity] Cons(e, t)) =>
    if (rel((a, e))) {
      [@implicit_arity] Cons(n, subst((rel, n, a, t)));
    } else {
      [@implicit_arity] Cons(e, subst((rel, n, a, t)));
    };

/* 33 */
let less_than = ((n: int, m: int)) => n < m;

let in_range = (((small, large), x)) =>
  less_than((small, x)) && less_than((x, large));

/* 40 */
let rec subst_pred =
  fun
  | (pred, n, Empty) => Empty
  | (pred, n, [@implicit_arity] Cons(e, t)) =>
    if (pred(e)) {
      [@implicit_arity] Cons(n, subst_pred((pred, n, t)));
    } else {
      [@implicit_arity] Cons(e, subst_pred((pred, n, t)));
    };

/* 48 */
let is_15 = x => eq_int((x, 15));

/* 52 */
let less_than_15 = x => less_than((x, 15));

/* 59 */
let in_range_11_16 = x => less_than((11, x)) && less_than((x, 16));

/* 65 */
let in_range_c = ((small, large), x) =>
  less_than((small, x)) && less_than((x, large));

/* 79 */
let rec subst_c = pred =>
  fun
  | (n, Empty) => Empty
  | (n, [@implicit_arity] Cons(e, t)) =>
    if (pred(e)) {
      [@implicit_arity] Cons(n, subst_c(pred, (n, t)));
    } else {
      [@implicit_arity] Cons(e, subst_c(pred, (n, t)));
    };

/* 88 */
let rec combine =
  fun
  | (Empty, Empty) => Empty
  | (Empty, [@implicit_arity] Cons(b, l2)) => [@implicit_arity] Cons(b, l2)
  | ([@implicit_arity] Cons(a, l1), Empty) => [@implicit_arity] Cons(a, l1)
  | ([@implicit_arity] Cons(a, l1), [@implicit_arity] Cons(b, l2)) =>
    [@implicit_arity] Cons(a, combine((l1, [@implicit_arity] Cons(b, l2))));

// no [@implicit_arity] still compiles and gets the rec recognized.
let rec combine2 =
  fun
  | (Empty, Empty) => Empty
  | (Empty,  Cons(b, l2)) =>  Cons(b, l2)
  | ( Cons(a, l1), Empty) =>  Cons(a, l1)
  | ( Cons(a, l1),  Cons(b, l2)) =>
     Cons(a, combine2((l1,  Cons(b, l2))));

let rec combine =
  fun
  | (Empty, l2) => l2
  | ([@implicit_arity] Cons(a, l1), l2) =>
    [@implicit_arity] Cons(a, combine((l1, l2)));

/* 93 */
let rec combine_c =
  fun
  | Empty => (l2 => l2)
  | [@implicit_arity] Cons(a, l1) => (
      l2 => [@implicit_arity] Cons(a, combine_c(l1, l2))
    );

/* 95 */
let prefixer_123 = l2 =>
  [@implicit_arity]
  Cons(1, [@implicit_arity] Cons(2, [@implicit_arity] Cons(3, l2)));

/* 97 */
let waiting_prefix_123 = l2 =>
  [@implicit_arity]
  Cons(
    1,
    combine_c(
      [@implicit_arity] Cons(2, [@implicit_arity] Cons(3, Empty)),
      l2,
    ),
  );

/* 104 */
let base = l2 => l2;

/* 115 */
let rec combine_s =
  fun
  | Empty => base
  | [@implicit_arity] Cons(a, l1) => make_cons((a, combine_s(l1)))
and make_cons = ((a, f), l2) => [@implicit_arity] Cons(a, f(l2));
