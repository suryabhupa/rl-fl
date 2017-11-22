open Core.Std

open Utils
open Type
open Program
open Enumeration
open Grammar


type task =
  { name: string; task_type: tp;
    log_likelihood: program -> float;
  }

let supervised_task name ty examples =
  { name = name;
    task_type = ty;
    log_likelihood = (fun p ->
        let f = evaluate [] p in
        if List.for_all ~f:(fun (x,y) ->
            try
              f x = y
            with _ -> false) examples
        then 0.0
        else log 0.0)
  }



let keep_best_programs_in_frontier (k : int) (f : frontier) : frontier =
 {programs =  List.sort ~cmp:(fun (_,a) (_,b) -> if a > b then 1 else -1) f.programs |> flip List.take k }

(* Takes a frontier and a task. Ads in the likelihood on the task to
   the frontier and removes things that didn't hit the task *)
let score_programs_for_task (f:frontier) (t:task) : frontier =
  {programs = f.programs |> List.filter_map ~f:(fun (program, descriptionLength) ->
       let likelihood = t.log_likelihood program in
       if likelihood > -0.1 then begin 
         Printf.printf "HIT: %s\n" t.name;
         Some((program, descriptionLength +. likelihood))
       end
       else None)
  }

let enumerate_solutions_for_tasks grammar tasks frontier_size ?keepTheBest:(keepTheBest = 0) : frontier list =
  (* what are all of the different types that we need to enumerate for *)
  let ts = List.map tasks ~f:(fun t -> t.task_type) |> List.dedup in
  
  (* the corresponding frontiers. [frontier] *)
  let fs = List.map ts ~f:(fun t -> iterative_deepening_enumeration grammar t frontier_size) in

  let entire_frontiers = 
  List.map tasks ~f:(fun t ->
      let j = List.findi ts ~f:(fun _ tp -> tp = t.task_type) |> get_some |> fst in
      let frontier = List.nth fs j |> get_some in
      score_programs_for_task frontier t)
  in
  match keepTheBest with
  | 0 -> entire_frontiers
  | k -> entire_frontiers |> List.map ~f:(keep_best_programs_in_frontier k)

let top_solutions_for_tasks k grammar tasks solutions =
  let top_solution_for_task t s =
    List.map s ~f:(fun p -> (likelihood_under_grammar grammar t.task_type p, p))
    |> List.sort ~cmp:(fun (l1,_) (l2,_) -> if l2 -. l1 > 0.0 then 1 else -1)
    |> flip List.take k
    |> List.map ~f:(fun (_,p) -> p)
  in List.map2_exn ~f:top_solution_for_task tasks solutions





let arithmetic_grammar =
  primitive_grammar [ primitive "k0" tint 0;
                      primitive "k1" tint 1;
                      primitive "+" (tint @> tint @> tint) (+);
                      primitive "*" (tint @> tint @> tint) ( * );
                      primitive "apply" (t0 @> (t0 @> t1) @> t1) (fun x f -> f x);]





