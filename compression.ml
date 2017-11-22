open Core.Std

open Utils
open Type
open Program
open Enumeration
open Task
open Grammar

module FreeMap = Map.Make(Int)

type fragment =
  | FIndex of int
  | FAbstraction of fragment
  | FApply of fragment*fragment
  | FPrimitive of tp * string
  | FVariable

exception FragmentFail

let rec bind_fragment context environment
    (f : fragment) (p : program) : tContext*tp*((tp*program) list)*((tp*program) FreeMap.t) =
  
  let combine context (l,m) (lp,mp) =
    let (context,free) = 
     FreeMap.fold2 m mp ~init:(context, FreeMap.empty)
       ~f:(fun ~key:key ~data:merge (context,accumulator) ->
         match merge with
         | `Both((t1,p1),(t2,p2)) when not (p1 <> p2) -> raise FragmentFail
         | `Both((t1,p1),(t2,p2)) -> begin 
             try
               let context = unify context t1 t2 in
               let (t,context) = chaseType context t1 in
               (context, FreeMap.add accumulator ~key:key ~data:(t,p1))
             with _ -> raise FragmentFail
           end
         | `Left(program_and_type) ->
           (context, FreeMap.add accumulator ~key:key ~data:program_and_type)
         | `Right(program_and_type) ->
           (context, FreeMap.add accumulator ~key:key ~data:program_and_type))
    in (context, l@lp, free)
    
  in
  
  let rec hoist ?d:(d = 0) j p = match p with
    | Primitive(_,_) -> p
    | Apply(f,x) -> Apply(hoist j f ~d:d, hoist j x ~d:d)
    | Abstraction(b) -> Abstraction(hoist j b ~d:(d+1))
    | Index(i) when i < d -> p (* bound within the hoisted code *)
    | Index(i) when i >= d+j -> Index(i - j) (* bound outside the hoisted code but also outside the fragment *)
    | Index(_) -> raise FragmentFail (* bound inside of the fragment and so cannot be hoisted *)
  in
  match (f,p) with
  | (FApply(a,b),Apply(m,n)) ->
    let (context, ft,holes1,free1) = bind_fragment context environment a m in
    let (context, xt,holes2,free2) = bind_fragment context environment b n in
    let (context, holes, free) = combine context (holes1,free1) (holes2,free2) in
    let (alpha, context) = makeTID context in
    let context =
      try
        unify context (xt @> alpha) ft
      with  _ -> raise FragmentFail
    in
    let (alpha, context) = chaseType context alpha in
    (context, alpha, holes, free)
    
  | (FPrimitive(t,n1),Primitive(_,n2)) when n1 = n2 ->
    let (t,context) = instantiate_type context t in
    (context,t,[],FreeMap.empty)
  | (FAbstraction(m),Abstraction(n)) ->
    let (alpha, context) = makeTID context in
    let (context,beta,holes,free) = bind_fragment context (alpha::environment) m n in
    (context, alpha @> beta, holes, free)
  | (FVariable, _) ->
    let (alpha, context) = makeTID context in
    let p = hoist (List.length environment) p in
    (context, alpha, [(alpha, p)], FreeMap.empty)
  | (FIndex(j),_) when j >= List.length environment ->
    let p = hoist (List.length environment) p in
    let (alpha, context) = makeTID context in
    (context, alpha, [],
     FreeMap.singleton (j - List.length environment) (alpha,p))
  | (FIndex(j),Index(k)) when j < List.length environment && j = k ->
    (context, List.nth_exn environment j, [], FreeMap.empty)
  | _ -> raise FragmentFail
    
    

let rec program_matches_fragment (p:program) (f:fragment) : (program list) option =
  match (f,p) with 
  | (FVariable,_) -> Some([p])
  | (FPrimitive(_,name),Primitive(_,namep)) when name = namep -> Some([]) 
  | (FPrimitive(_,name),_) -> None
  | (FApply(a,b),Apply(m,n)) -> begin 
      match program_matches_fragment m a with 
      | None -> None 
      | Some(prefix) -> match program_matches_fragment n b with
        | None -> None 
        | Some(suffix) -> Some(prefix@suffix) 
    end
  | (FAbstraction(m),Abstraction(n)) ->
    program_matches_fragment n m
  | (FIndex(_),Index(_)) -> Some([])
  | _ -> None
    
(*     (\* Referencing a variable bound inside of the fragment *\) *)
(*     | (FIndex(j),Index(k)) when j = k && j < abstraction_depth -> Some([]) *)
(*     (\* Referencing a free variable in the fragment *\) *)
(*     | (FIndex(j),_) when j >= abstraction_depth -> *)
(*       begin *)
(*         match Hashtbl.Poly.find bindings (j - abstraction_depth)  with *)
(*         | Some(pp) *)
(*       end *)
(*       Some([p]) *)
    

let rec string_of_fragment = function
  | FIndex(j) -> "$" ^ string_of_int j
  | FAbstraction(body) ->
    "(lambda "^string_of_fragment body^")"
  | FApply(p,q) ->
    "("^string_of_fragment p^" "^string_of_fragment q^")"
  | FPrimitive(_,n) -> n
  | FVariable -> "??"

let close_fragment (f:fragment) : program =
  (* Mapping from <index beyond this fragment>, <which lambda it refers to, starting at 0> *)
  let mapping = Hashtbl.Poly.create () in
  let number_of_abstractions = ref 0 in
  let rec walk d = function
    | FVariable -> begin incr number_of_abstractions; Index(!number_of_abstractions + d - 1) end
    | FPrimitive(t,n) -> Primitive(t,n)
    | FAbstraction(b) -> Abstraction(walk (d+1) b)
    | FApply(f,x) -> Apply(walk d f, walk d x)
    | FIndex(j) when j < d -> Index(j)
    | FIndex(j) ->
      match Hashtbl.Poly.find mapping (j - d) with
      | Some(lambda_index) -> Index(lambda_index + d)
      | None -> begin
          incr number_of_abstractions;
          ignore(Hashtbl.Poly.add mapping (j - d) (!number_of_abstractions - 1));
          Index(!number_of_abstractions + d - 1)
        end
  in
  let body = walk 0 f in
  List.fold_left (0--(!number_of_abstractions - 1)) ~init:body ~f:(fun b _ -> Abstraction(b))

let rec fragments (d:int) (a:int) (p:program) =
  (* All of the fragments that have exactly a variables *)
  (* Assumed that we are in an environment with d lambda expressions *)
  let recursiveFragments = 
    match p with
    | Apply(f,x) ->
      List.map (0--a) ~f:(fun fa ->
          let functionFragments = fragments d fa f in
          let argumentFragments = fragments d (a-fa) x in
          List.cartesian_product functionFragments argumentFragments |>
          List.map ~f:(fun (fp,xp) -> FApply(fp,xp))) |>
      List.concat
    | Index(j) when a = 0 -> [FIndex(j)]
    | Index(j) -> []
    | Abstraction(body) ->
      fragments (d+1) a body |> List.map ~f:(fun b -> FAbstraction(b))
    | Primitive(t,n) when a = 0 -> [FPrimitive(t,n)]
    | Primitive(t,n) -> []
  in
  (* Given that there are d surrounding lambdas in the thing we are
     trying to fragment, and e surrounding lambdas in the fragmented
     self, are we allowed to replace it with a fragment?  The idea
     here is that any variable has to NOT refer to anything bound in
     the larger program which is not bound in the fragment*)
  let rec refers_to_bound_variables e = function
    | Apply(f,x) -> refers_to_bound_variables e f || refers_to_bound_variables e x
    | Abstraction(b) -> refers_to_bound_variables (e + 1) b
    | Index(j) -> j > e - 1 && (* has to not refer to something bound in the fragment *)
                  j < e + d (* has to not refer to something outside the whole program body *)
    | Primitive(_,_) -> false
  in
  if a = 1 && (not (refers_to_bound_variables 0 p)) then FVariable :: recursiveFragments else recursiveFragments

let is_fragment_nontrivial f =
  let rec variables = function
    | FVariable -> 1
    | FAbstraction(b) -> variables b
    | FApply(f,x) -> variables f + variables x
    | _ -> 0
  in
  let rec primitives = function
    | FPrimitive(_,_) -> 1
    | FAbstraction(b) -> primitives b
    | FApply(f,x) -> primitives f + primitives x
    | _ -> 0
  in
  Float.of_int (primitives f) +. (0.5 *. (Float.of_int (variables f))) > 1.5
     
let rec propose_fragments (a:int) (program:program) : fragment list =
  let recursiveFragments =
    match program with
    | Apply(f,x) -> propose_fragments a f @ propose_fragments a x
    | Abstraction(b) -> propose_fragments a b
    | _ -> []
  in
  recursiveFragments @ (List.map (0--a) ~f:(fun ap -> fragments 0 ap program) |> List.concat
                        |> List.filter ~f:is_fragment_nontrivial)
   
let rec propose_fragments_from_frontiers (a:int) (frontiers: frontier list) : fragment list =
  let from_each = frontiers |> List.map ~f:(fun f ->
      f.programs |> List.map ~f:(fun (p,_) -> propose_fragments a p) |> List.concat |> remove_duplicates) in
  let counts = Hashtbl.Poly.create() in
  List.iter from_each ~f:(List.iter ~f:(fun f ->
      match Hashtbl.find counts f with
      | Some(k) -> Hashtbl.replace counts f (k + 1)
      | None -> ignore(Hashtbl.add counts f 1)));
  let thisa = Hashtbl.to_alist counts |> List.filter_map ~f:(fun (f,k) ->
      if k > 1 then Some(f) else None)
  in
  if a = 0 then thisa else thisa@(propose_fragments_from_frontiers (a - 1) frontiers)

let rec program_cost_with_fragments (fragments : fragment list) (p : program) : int =
  let recursive_costs = fragments |> List.map ~f:(fun f ->
      match program_matches_fragment p f with
      | None -> None
      | Some(children) -> Some(1 + sum (List.map ~f:(program_cost_with_fragments fragments) children)))
  in
  let basics_cost = match p with
    | Abstraction(b) -> 1 + program_cost_with_fragments fragments b
    | Apply(f,x) -> program_cost_with_fragments fragments f + program_cost_with_fragments fragments x
    | _ -> 1 (* variable or other terminal *)
  in
  List.filter (Some(basics_cost) :: recursive_costs) ~f:is_some |>
  List.map ~f:get_some |> minimum

let induce_fragments (candidates : fragment list) (frontiers : frontier list) =
  let frontiers = frontiers |> List.filter ~f:(fun f -> List.length f.programs > 0) in
  let fitness candidate = 
    frontiers |> List.map ~f:(fun f ->
        f.programs |> List.map ~f:(fun (p,_) ->
            program_cost_with_fragments candidate p)
          |> minimum) |> sum
  in
  Printf.printf "Initial fitness: %d" (fitness []);
  let best = ref ([],fitness []) in
  for step = 1 to 5 do
    Printf.printf "STEP %d" step;
    let extensions = candidates |> List.map ~f:(fun k -> k::(fst !best)) in
    let scores = extensions |> List.map ~f:fitness |> List.zip extensions |> get_some in
    let (bestExtension,bestFitness) = scores |> maximum_by ~cmp:(fun (_,k1) (_,k2) -> k2 - k1) in
    Printf.printf "Decided to add the fragment %s\n" (string_of_fragment @@ List.hd_exn bestExtension);
    Printf.printf "New fitness: %d\n" bestFitness;
    best := (bestExtension,bestFitness);
  done
  ;
  fst (!best)

  

let main() =
  let p = Apply(Abstraction(Apply(Index(2),Index(0))),Index(1)) in
  [FApply(FIndex(0),FAbstraction(FIndex(99)))] @ propose_fragments 1 p |>
  List.map ~f:(fun f -> Printf.printf "%s\t%s\n" (string_of_fragment f) (close_fragment f |> show_program))
