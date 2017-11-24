open Core.Std

open Utils
open Type

type program =
  | Index of int
  | Abstraction of program
  | Apply of program*program
  | Primitive of tp * string

let is_index = function
  |Index(_) -> true
  |_ -> false

let program_children = function
  | Abstraction(b) -> [b]
  | Apply(m,n) -> [m;n]
  | _ -> []

let rec program_size p =
  1 + (List.map ~f:program_size (program_children p) |> sum)

let rec program_subexpressions p =
  p::(List.map (program_children p) program_subexpressions |> List.concat)

let rec show_program = function
  | Index(j) -> "$" ^ string_of_int j
  | Abstraction(body) ->
    "(lambda "^show_program body^")"
  | Apply(p,q) ->
    "("^show_program p^" "^show_program q^")"
  | Primitive(_,n) -> n

let string_of_program = show_program

let rec infer_program_type context environment = function
  | Index(j) ->
    let (t,context) = List.nth_exn environment j |> chaseType context in (context,t)
  | Primitive(t,_) -> let (t,context) = instantiate_type context t in (context,t)
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
    

let lookup_primitive_callback =
  ref (fun n ->
      raise (Failure ("unknown primitive "^n)))
    
let primitive n t p =
  (try
    !lookup_primitive_callback n
  with _ ->
    let old_callback = !lookup_primitive_callback in
    Printf.printf "Registering primitive %s\n" n;
    lookup_primitive_callback :=
      fun np -> if n = np then magical p else old_callback np);
  Primitive(t, n)

let lookup_primitive  = function
  | "k0" -> magical 0
  | "k1" -> magical 1
  | "k2" -> magical 2
  | "k3" -> magical 3
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
  | n -> raise (Failure ("unknown primitive: "^n))
                   
let rec evaluate (environment: 'b list) (p:program) : 'a =
  match p with
  | Abstraction(b) -> magical @@ fun argument -> evaluate (argument::environment) b
  | Index(j) -> magical @@ List.nth_exn environment j
  | Apply(f,x) -> (magical @@ evaluate environment f) (magical @@ evaluate environment x)
  | Primitive(_,n) -> (* !lookup_primitive_callback *) lookup_primitive n |> magical

let rec remove_abstractions (n : int) (q : program) : program =
  match (n,q) with
  | (0,q) -> q
  | (n,Abstraction(body)) -> remove_abstractions (n - 1) body
  | _ -> raise (Failure "remove_abstractions")


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
             
