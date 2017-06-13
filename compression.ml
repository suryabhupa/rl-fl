open Core.Std

open Utils
open Type
open Program
open Enumeration
open Task

type fragment =
  | FIndex of int
  | FAbstraction of fragment
  | FApply of fragment*fragment
  | FPrimitive of tp * string
  | FConditional of fragment*fragment*fragment
  | FVariable

let rec string_of_fragment = function
  | FIndex(j) -> string_of_int j
  | FAbstraction(body) ->
    "(lambda "^string_of_fragment body^")"
  | FApply(p,q) ->
    "("^string_of_fragment p^" "^string_of_fragment q^")"
  | FPrimitive(_,n) -> n
  | FConditional(t,y,n) -> "(if "^string_of_fragment t^" "^string_of_fragment y^" "^string_of_fragment n^")"
  | FVariable -> "??"

let rec fragments (a:int) (p:program) =
  let recursiveFragments =  
    match p with
    | Apply(f,x) ->
      List.map (0--a) ~f:(fun fa ->
          let functionFragments = fragments fa f in
          let argumentFragments = fragments (a-fa) x in
          List.cartesian_product functionFragments argumentFragments |>
          List.map ~f:(fun (fp,xp) -> FApply(fp,xp))) |>
      List.concat
    | Index(j) -> [FIndex(j)]
    | Abstraction(body) ->
      fragments a body |> List.map ~f:(fun b -> FAbstraction(b))
    | Primitive(t,n) ->
      [FPrimitive(t,n)]
    | Conditional(t,y,n) ->
      (0--a) |> List.map ~f:(fun ta ->
          let testFragments = fragments ta t in
          (0--(a - ta)) |> List.map ~f:(fun ya ->
              let yesFragments = fragments ya y in
              let na = a - ta - ya in
              let noFragments = fragments na n in
              List.cartesian_product testFragments (List.cartesian_product yesFragments noFragments) |>
              List.map ~f:(fun (tp,(yp,np)) -> FConditional(tp,yp,np))))
    |> List.concat  |> List.concat 
  in
  if a = 1 then FVariable :: recursiveFragments else recursiveFragments
   

let _ =
  let p = Conditional(Index(1),Apply(Index(0),Index(1)),Apply(Index(0),Index(2))) in
  fragments 1 p |>
  List.map ~f:(print_string%string_of_fragment)
