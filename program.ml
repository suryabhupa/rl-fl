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
  | n -> raise (Failure "unknown primitive")
                   
let rec evaluate (environment: 'b list) (p:program) : 'a =
  match p with
  | Abstraction(b) -> magical @@ fun argument -> evaluate (argument::environment) b
  | Index(j) -> magical @@ List.nth_exn environment j
  | Apply(f,x) -> (magical @@ evaluate environment f) (magical @@ evaluate environment x)
  | Primitive(_,n) -> (* !lookup_primitive_callback *) lookup_primitive n |> magical




