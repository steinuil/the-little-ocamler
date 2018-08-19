(* 1 *)
type pizza =
    Crust
  | Cheese of pizza
  | Onion of pizza
  | Anchovy of pizza
  | Sausage of pizza

(* 31 *)
let rec remove_anchovy =
  function
    Crust -> Crust
  | Cheese x -> Cheese (remove_anchovy x)
  | Onion x -> Onion (remove_anchovy x)
  | Anchovy x -> remove_anchovy x
  | Sausage x -> Sausage (remove_anchovy x)

(* 43 *)
let rec top_anchovy_with_cheese =
  function
    Crust -> Crust
  | Cheese x -> Cheese (top_anchovy_with_cheese x)
  | Onion x -> Onion (top_anchovy_with_cheese x)
  | Anchovy x -> Cheese (Anchovy (top_anchovy_with_cheese x))
  | Sausage x -> Sausage (top_anchovy_with_cheese x)

(* 49 *)
let subst_anchovy_by_cheese x =
  remove_anchovy (top_anchovy_with_cheese x)

(* 53 *)
let rec subst_anchovy_by_cheese =
  function
    Crust -> Crust
  | Cheese x -> Cheese (subst_anchovy_by_cheese x)
  | Onion x -> Onion (subst_anchovy_by_cheese x)
  | Anchovy x -> Cheese (subst_anchovy_by_cheese x)
  | Sausage x -> Sausage (subst_anchovy_by_cheese x)
