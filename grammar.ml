open Core.Std

open Utils
open Type
open Program



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

let unifying_expressions g environment request context : (program*tp*tContext*float) list =
  (* given a grammar environment requested type and typing context,
     what are all of the possible leaves that we might use?
     These could be productions in the grammar or they could be variables. *)
  let candidates = g.library @ List.mapi ~f:(fun j t -> (Index(j),t,g.logVariable)) environment in
  let candidates = 
  List.filter_map ~f:(fun (p,t,ll) ->
        try
          let (t,context) = if not (is_index p) then instantiate_type context t else (t,context) in
        match arguments_and_return_of_type t with
        | (argument_types, return_type) -> 
          let newContext = unify context return_type request in
          Some(p, t, newContext,ll)
      with _ -> None) candidates
  in
  let z = List.map ~f:(fun (_,_,_,ll) -> ll) candidates |> lse_list in
  List.map ~f:(fun (p,t,k,ll) -> (p,t,k,ll-.z)) candidates


let likelihood_under_grammar g request p = 0.0
(*  let rec likelihood (r : tp) (environment: tp list) (context: tContext) =
    match arguments_and_return_of_type r with
    | ([],_) -> (* not a function *)
      let candidates = unifying_expressions g environment request context in
      
      List.map ~f:(fun (candidate, candidate_type, context, ll) ->
          enumerate_applications g context candidate_type candidate environment (depth+.ll) |>
        List.map ~f:(fun (p,k,al) -> (p,k,ll+.al)))
      |> List.concat

*)
