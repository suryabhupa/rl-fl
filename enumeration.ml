open Core.Std

open Utils
open Type
open Program
open Grammar


let rec enumerate_programs (g: grammar) (context: tContext) (request: tp) (environment: tp list) (depth: float) : (program*tContext*float) list =
  (*   Printf.printf "DEPTH: %f\n" depth; *)
  if depth < 0.0 then [] else
    match arguments_and_return_of_type request with
    | ([],_) -> (* not trying to enumerate functions *)
      unifying_expressions g environment request context |> 
      List.map ~f:(fun (candidate, candidate_type, context, ll) ->
          enumerate_applications g context candidate_type candidate environment (depth+.ll) |>
        List.map ~f:(fun (p,k,al) -> (p,k,ll+.al)))
      |> List.concat
    | (arguments,return) ->
      let newEnvironment = List.rev arguments @ environment in
      let ad_lambdas b = List.fold_left ~init:b ~f:(fun b _ -> Abstraction(b)) (1 -- List.length arguments) in
      enumerate_programs g context return newEnvironment depth |>
      List.map ~f:(fun (body, newContext, ll) -> (ad_lambdas body, newContext, ll))
and
  enumerate_applications (g: grammar) (context: tContext) (f_type: tp) (f: program) (environment: tp list) (depth: float) : (program*tContext*float) list =
  (* returns the log likelihood of the arguments! not the log likelihood of the application! *)
  match arguments_and_return_of_type f_type with
  | ([], _) -> (* not a function so we don't need any applications *)
    [(f, context, 0.0)]
  | (first_argument::rest_arguments, _) ->
    enumerate_programs g context first_argument environment depth |>
    List.map ~f:(fun (a,k,ll) ->
        let a = Apply(f,a) in
        let (applicationType,k) = chaseType k (right_of_arrow f_type) in
        enumerate_applications g k applicationType a environment (depth+.ll) |>
      List.map ~f:(fun (a,k,a_ll) -> (a,k,a_ll+.ll))) |>
    List.concat

let iterative_deepening_enumeration (g:grammar) (request:tp) (size:int) : (program list) =
  let startTime = Time.now () in
  let rec deepen bound =
    let possibleSolutions = enumerate_programs g empty_context request [] bound in
    if List.length possibleSolutions<size then deepen (bound +. 1.0) else
      List.map ~f:(fun (p,_,_) -> p) possibleSolutions
  in
  let result = deepen 1.0 in
  Printf.printf "Enumerated %d programs of type %s in time %s\n"
    (List.length result)
    (string_of_type request)
    (Time.diff (Time.now ()) startTime |> Core.Span.to_string);
  result


