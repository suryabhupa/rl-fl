open Core.Std

open Utils
open Type

type program =
  | Index of int
  | Abstraction of program
  | Apply of program*program
  | Primitive of tp * string
  | Invented of tp * program

let is_index = function
  |Index(_) -> true
  |_ -> false

let is_primitive = function
  |Primitive(_,_) -> true
  |Invented(_,_) -> true
  |_ -> false

let program_children = function
  | Abstraction(b) -> [b]
  | Apply(m,n) -> [m;n]
  | _ -> []

let rec program_size p =
  1 + (List.map ~f:program_size (program_children p) |> sum)

let rec program_subexpressions p =
  p::(List.map (program_children p) program_subexpressions |> List.concat)

let rec show_program (is_function : bool) = function
  | Index(j) -> "$" ^ string_of_int j
  | Abstraction(body) ->
    "(lambda "^show_program false body^")"
  | Apply(p,q) ->
    if is_function then
      show_program true p^" "^show_program false q
    else
      "("^show_program true p^" "^show_program false q^")"
  | Primitive(_,n) -> n
  | Invented(_,i) -> "#"^show_program false i

let string_of_program = show_program false

let rec infer_program_type context environment = function
  | Index(j) ->
    let (t,context) = List.nth_exn environment j |> chaseType context in (context,t)
  | Primitive(t,_) -> let (t,context) = instantiate_type context t in (context,t)
  | Invented(t,_) -> let (t,context) = instantiate_type context t in (context,t)
  | Abstraction(b) ->
    let (xt,context) = makeTID context in
    let (context,rt) = infer_program_type context (xt::environment) b in
    let (ft,context) = chaseType context (xt @> rt) in
    (context,ft)
  | Apply(f,x) ->
    let (rt,context) = makeTID context in
    let (context, xt) = infer_program_type context environment x in
    let (context, ft) = infer_program_type context environment f in
    let context = unify context ft (xt @> rt) in
    let (rt, context) = chaseType context rt in
    (context, rt)

exception UnknownPrimitive of string

let lookup_primitive  = function
  | "k0" -> magical 0
  | "k1" -> magical 1
  | "k2" -> magical 2
  | "k3" -> magical 3
  | "k4" -> magical 4
  | "k5" -> magical 5
  | "k6" -> magical 6
  | "k7" -> magical 7
  | "k8" -> magical 8
  | "k9" -> magical 9
  | "+" -> magical (+)
  | "-" -> magical (-)
  | "*" -> magical ( * )
  | "apply" -> magical (fun x f -> f x)
  | "map" -> magical (fun f l -> List.map ~f:f l)
  | "sort" -> magical (List.sort ~cmp:(fun x y -> x - y))
  | "reverse" -> magical List.rev
  | "append" -> magical (@)
  | "singleton" -> magical (fun x -> [x])
  | "slice" -> magical slice
  | "length" -> magical List.length
  | "filter" -> magical (fun f l -> List.filter ~f:f l)
  | "fold_right" -> magical (fun f x0 l -> List.fold_right ~f:f ~init:x0 l)
  | "eq?" -> magical (fun x y -> x = y)
  | "gt?" -> magical (fun (x : int) (y : int) -> x > y)
  | "not" -> magical (fun x -> not x)
  | n -> raise (UnknownPrimitive n)
                   
let rec evaluate (environment: 'b list) (p:program) : 'a =
  match p with
  | Abstraction(b) -> magical @@ fun argument -> evaluate (argument::environment) b
  | Index(j) -> magical @@ List.nth_exn environment j
  | Apply(f,x) -> (magical @@ evaluate environment f) (magical @@ evaluate environment x)
  | Primitive(_,n) -> lookup_primitive n
  | Invented(_,i) -> evaluate [] i

let rec remove_abstractions (n : int) (q : program) : program =
  match (n,q) with
  | (0,q) -> q
  | (n,Abstraction(body)) -> remove_abstractions (n - 1) body
  | _ -> raise (Failure "remove_abstractions")


(* PRIMITIVES *)
let primitive (name : string) (t : tp) _ = Primitive(t,name)

let primitive0 = primitive "k0" tint 0;;
let primitive1 = primitive "k1" tint 1;;
let primitive2 = primitive "k2" tint 2;;
let primitive3 = primitive "k3" tint 3;;
let primitive4 = primitive "k4" tint 4;;
let primitive5 = primitive "k5" tint 5;;
let primitive6 = primitive "k6" tint 6;;
let primitive7 = primitive "k7" tint 7;;
let primitive8 = primitive "k8" tint 8;;
let primitive9 = primitive "k9" tint 9;;
let primitive_addition = primitive "+" (tint @> tint @> tint) (+);;
let primitive_subtraction = primitive "-" (tint @> tint @> tint) (-);;
let primitive_multiplication = primitive "*" (tint @> tint @> tint) ( * );;

let primitive_apply = primitive "apply" (t1 @> (t1 @> t0) @> t0) (fun x f -> f x);;

let primitive_sort = primitive "sort" (tlist tint @> tlist tint) (List.sort ~cmp:(fun x y -> x - y));;
let primitive_reverse = primitive "reverse" (tlist tint @> tlist tint) (List.rev);;
let primitive_append = primitive "append"  (tlist tint @> tlist tint @> tlist tint) (@);;
let primitive_singleton = primitive "singleton"  (tint @> tlist tint) (fun x -> [x]);;
let primitive_slice = primitive "slice" (tint @> tint @> tlist tint @> tlist tint) slice;;
let primitive_length = primitive "length" (tlist tint @> tint) (List.length);;
let primitive_map = primitive "map" ((tint @> tint) @> (tlist tint) @> (tlist tint)) (fun f l -> List.map ~f:f l);;
let primitive_fold_right = primitive "fold_right" ((tint @> tint @> tint) @> tint @> (tlist tint) @> tint) (fun f x0 l -> List.fold_right ~f:f ~init:x0 l);;
let primitive_filter = primitive "filter" ((tint @> tboolean) @> (tlist tint) @> (tlist tint)) (fun f l -> List.filter ~f:f l);;
let primitive_equal = primitive "eq?" (tint @> tint @> tboolean) ( = );;
let primitive_not = primitive "not" (tboolean @> tboolean) (not);;
let primitive_greater_than = primitive "gt?" (tint @> tint @> tboolean) (fun (x: int) (y: int) -> x > y);;

let test_program_inference program desired_type =
  let (context,t) = infer_program_type empty_context [] program in
  let (t,_) = chaseType context t in
  let t = canonical_type t in
  Printf.printf "%s : %s\n" (string_of_program program) (string_of_type t);
  assert (t = (canonical_type desired_type))

let program_test_cases() =
  test_program_inference (Abstraction(Index(0))) (t0 @> t0);
  test_program_inference (Abstraction(Abstraction(Apply(Index(0),Index(1))))) (t0 @> (t0 @> t1) @> t1);
  test_program_inference (Abstraction(Abstraction(Index(1)))) (t0 @> t1 @> t0);
  test_program_inference (Abstraction(Abstraction(Index(0)))) (t0 @> t1 @> t1);
    
;;
(* program_test_cases() *)
             
