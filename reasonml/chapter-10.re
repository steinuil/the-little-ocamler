let eq_int = ((n: int, m: int)) => n == m;

/* 4 */
exception Too_small;

let is_zero = n => eq_int((n, 0));

let pred = n =>
  if (is_zero(n)) {
    raise(Too_small);
  } else {
    n - 1;
  };

let succ = n => n + 1;

let rec plus = ((n, m)) =>
  if (is_zero(n)) {
    m;
  } else {
    succ(plus((pred(n), m)));
  };

/* 5 */
type num =
  | Zero
  | One_more_than(num);

let is_zero =
  fun
  | Zero => true
  | not_zero => false;

let pred =
  fun
  | Zero => raise(Too_small)
  | One_more_than(n) => n;

let succ = n => One_more_than(n);

let rec plus = ((n, m)) =>
  if (is_zero(n)) {
    m;
  } else {
    succ(plus((pred(n), m)));
  };

/* 18 */
module type N = {
  type number;
  exception Too_small;
  let succ: number => number;
  let pred: number => number;
  let is_zero: number => bool;
};

/* 26 */
module NumberAsNum0 = (()) : N => {
  type num =
    | Zero
    | One_more_than(num);
  type number = num;
  exception Too_small;
  let succ = n => One_more_than(n);
  let pred =
    fun
    | Zero => raise(Too_small)
    | One_more_than(n) => n;
  let is_zero =
    fun
    | Zero => true
    | a_num => false;
};

module NumberAsInt0 = (()) : N => {
  type number = int;
  exception Too_small;
  let succ = n => n + 1;
  let pred = n =>
    if (eq_int((n, 0))) {
      raise(Too_small);
    } else {
      n - 1;
    };
  let is_zero = n => eq_int((n, 0));
};

/* 33 */
module IntStruct0 =
  NumberAsInt0({});

/* 35 */
module NumStruct0 =
  NumberAsNum0({});

/* 40 */
module type P = {
  type number;
  let plus: ((number, number)) => number;
};

/* 41 */
module PON = (Na: N) : P => {
  type number = Na.number;
  let rec plus = ((n, m)) =>
    if (Na.is_zero(m)) {
      m;
    } else {
      Na.succ(plus((Na.pred(n), m)));
    };
};

/* 47 */
module IntArith0 = PON(IntStruct0);

/* 62 */
module type N_C_R = {
  type number;
  exception Too_small;
  let conceal: int => number;
  let reveal: number => int;
  let is_zero: number => bool;
  let succ: number => number;
  let pred: number => number;
};

/* 65 */
module NumberAsInt = (()) : N_C_R => {
  type number = int;
  exception Too_small;
  let conceal = n => n;
  let reveal = n => n;
  let is_zero = n => eq_int((n, 0));
  let succ = n => n + 1;
  let pred = n =>
    if (is_zero(n)) {
      raise(Too_small);
    } else {
      n - 1;
    };
};

module NumberAsNum = (()) : N_C_R => {
  type number =
    | Zero
    | One_more_than(number);
  exception Too_small;
  let rec conceal = n =>
    if (eq_int((n, 0))) {
      Zero;
    } else {
      One_more_than(conceal(n - 1));
    };
  let rec reveal =
    fun
    | Zero => 0
    | One_more_than(n) => 1 + reveal(n);
  let is_zero =
    fun
    | Zero => true
    | _ => false;
  let succ = n => One_more_than(n);
  let pred =
    fun
    | Zero => raise(Too_small)
    | One_more_than(n) => n;
};

/* 66 */
module IntStruct =
  NumberAsInt({});

module IntArith = PON(IntStruct);

module NumStruct =
  NumberAsNum({});

module NumArith = PON(NumStruct);

/* 93 */
module NumberAsInt2 = (()) : (N with type number = int) => {
  type number = int;
  exception Too_small;
  let is_zero = n => eq_int((n, 0));
  let succ = n => n + 1;
  let pred = n =>
    if (is_zero(n)) {
      raise(Too_small);
    } else {
      n - 1;
    };
};

/* 95 */
module IntStruct2 =
  NumberAsInt2({});

/* 96 */
module IntArith2 = PON(IntStruct2);

/* 114 */
module type S = {
  type number1;
  type number2;
  let similar: ((number1, number2)) => bool;
};

/* 115 */
module Same =
       (Na: N, Nb: N)
       : (S with type number1 = Na.number with type number2 = Nb.number) => {
  type number1 = Na.number;
  type number2 = Nb.number;
  let rec sim = ((n, m)) =>
    if (Na.is_zero(n)) {
      Nb.is_zero(m);
    } else {
      sim((Na.pred(n), Nb.pred(m)));
    };
  let similar = ((n, m)) =>
    try (sim((n, m))) {
    | Na.Too_small => false
    | Nb.Too_small => false
    };
};

/* 118 */
module SimIntNum = Same(IntStruct, NumStruct);

/* 119 */
module SimNumInt = Same(NumStruct, IntStruct);

/* 127 */
/* Can't reproduce in OCaml */

/* 129 */
module type J = {let new_plus: ((int, int)) => int;};

module NP =
       (Na: N_C_R with type number = int, Pa: P with type number = int)
       : J => {
  let new_plus = ((x, y)) =>
    Na.reveal(Pa.plus((Na.conceal(x), Na.conceal(y))));
};

/* Can't reproduce */
module NPStruct =
  NP(
    {
      type number = int;
      exception Too_small;
      let conceal = n => n;
      let reveal = n => n;
      let is_zero = n => eq_int((n, 0));
      let succ = n => n + 1;
      let pred = n =>
        if (is_zero(n)) {
          raise(Too_small);
        } else {
          n - 1;
        };
    },
    {
      type number = int;
      let rec plus = ((x, y)) => x + y;
    },
  );

/* 147 */
module type T = {
  type number;
  let times: ((number, number)) => number;
};

/*
 module TON (Na : N) (Pa : P)
     : T with type number = Na.number
   = struct
     type number = Na.number
     let rec times (n, m) =
       if Na.is_zero m
       then m
       else Pa.plus (n, times (n, Na.pred m))
   end
 */
