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

let grammar_primitives g =
  {logVariable = g.logVariable;
   library = g.library |> List.filter ~f:(fun (p,_,_) -> is_primitive p)}

let string_of_grammar g =
  string_of_float g.logVariable ^ "\n" ^
  join ~separator:"\n" (g.library |> List.map ~f:(fun (p,t,l) -> Float.to_string l^"\t"^(string_of_type t)^"\t"^(string_of_program p)))

let unifying_expressions g environment request context : (program*tp*tContext*float) list =
  (* given a grammar environment requested type and typing context,
     what are all of the possible leaves that we might use?
     These could be productions in the grammar or they could be variables. 
     Yields a sequence of:
     (leaf, instantiatedTypeOfLeaf, context with leaf type unified with requested type, normalized log likelihood)
*)
  let candidates = g.library @ List.mapi ~f:(fun j t -> (Index(j),t,g.logVariable)) environment in
  let candidates = 
  List.filter_map ~f:(fun (p,t,ll) ->
        try
          let (t,context) = if not (is_index p) then instantiate_type context t else (t,context) in
          match arguments_and_return_of_type t with
          | (argument_types, return_type) -> 
            let newContext = unify context return_type request in
            Some(p, t, newContext,ll)
        with UnificationFailure -> None) candidates
  in
  let z = List.map ~f:(fun (_,_,_,ll) -> ll) candidates |> lse_list in
  List.map ~f:(fun (p,t,k,ll) -> (p,t,k,ll-.z)) candidates


let likelihood_under_grammar g request expression =
  let rec walk_application_tree tree =
    match tree with
    | Apply(f,x) -> walk_application_tree f @ [x]
    | _ -> [tree]
  in
  
  let rec likelihood (r : tp) (environment: tp list) (context: tContext) (p: program) : (float*tContext) =
    match r with
    (* a function - must start out with a sequence of lambdas *)
    | TCon("->",[argument;return_type]) -> 
      let newEnvironment = argument :: environment in
      let body = remove_abstractions 1 p in
      likelihood return_type newEnvironment context body
    | _ -> (* not a function - must be an application instead of a lambda *)
      let candidates = unifying_expressions g environment r context in
      match walk_application_tree p with
      | [] -> raise (Failure "walking the application tree")
      | f::xs ->
        match List.find candidates ~f:(fun (candidate,_,_,_) -> candidate = f) with
        | None -> raise (Failure ("could not find function in grammar: "^(string_of_program p)))
        | Some(_, f_t, newContext, functionLikelihood) ->
          let (f_t, newContext) = chaseType newContext f_t in
          let (argument_types, _) = arguments_and_return_of_type f_t in
          List.fold_right (List.zip_exn xs argument_types)
            ~init:(functionLikelihood,newContext)
            ~f:(fun (x,x_t) (ll,ctx) ->
                let (x_ll,ctx) = likelihood x_t environment ctx x in
                (ll+.x_ll,ctx))
  in
  
  likelihood request [] empty_context expression |> fst

