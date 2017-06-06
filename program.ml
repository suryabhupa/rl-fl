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

let rec evaluate (environment: 'b list) (p:program) : 'a =
  match p with
  | Abstraction(b) -> magical @@ fun argument -> evaluate (argument::environment) b
  | Index(j) -> magical @@ List.nth_exn environment j
  | Apply(f,x) -> (magical @@ evaluate environment f) (magical @@ evaluate environment x)
  | Conditional(t,y,n) ->
    if magical @@ evaluate environment t
    then evaluate environment y else evaluate environment n
  | Primitive(_,"k0") -> magical 0
  | Primitive(_,"k1") -> magical 1
  | Primitive(_,"+") -> magical (+)
  | Primitive(_,"*") -> magical ( * )
  | Primitive(_,"apply") -> magical (fun x f -> f x)
  | Primitive(_,"map") -> magical (fun f l -> List.map ~f:f l)

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
