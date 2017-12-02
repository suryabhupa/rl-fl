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
  | "eq?" -> magical (fun x y -> x = y)
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
let primitive_addition = primitive "+" (tint @> tint @> tint) (+);;
let primitive_multiplication = primitive "*" (tint @> tint @> tint) ( * );;

let primitive_apply = primitive "apply" (t1 @> (t1 @> t0) @> t0) (fun x f -> f x);;


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
             
