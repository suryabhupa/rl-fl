open Core.Std

open Utils
open Type

type program =
  | Index of int
  | Abstraction of program
  | Apply of program*program
  | Primitive of tp * string
  | Conditional of program*program*program

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
  | Index(j) -> string_of_int j
  | Abstraction(body) ->
    "(lambda "^show_program body^")"
  | Apply(p,q) ->
    "("^show_program p^" "^show_program q^")"
  | Primitive(_,n) -> n
  | Conditional(t,y,n) -> "(if "^show_program t^" "^show_program y^" "^show_program n^")"

let primitive n t p = Primitive(t, n)

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
  | n -> raise (Failure ("unknown primitive "^n))
                   
let rec evaluate (environment: 'b list) (p:program) : 'a =
  match p with
  | Abstraction(b) -> magical @@ fun argument -> evaluate (argument::environment) b
  | Index(j) -> magical @@ List.nth_exn environment j
  | Apply(f,x) -> (magical @@ evaluate environment f) (magical @@ evaluate environment x)
  | Conditional(t,y,n) ->
    if magical @@ evaluate environment t
    then evaluate environment y else evaluate environment n
  | Primitive(_,n) -> lookup_primitive n




type grammar = {
  logVariable: float;
  library: (program*tp*float) list;
}

let primitive_grammar primitives =
  {library = List.map primitives ~f:(fun p -> match p with
       |Primitive(t,_) -> (p,t, 0.0 -. (log (float_of_int (List.length primitives)))));
   logVariable = log 0.5
  }



let string_of_grammar g =
  string_of_float g.logVariable ^ "\n"
