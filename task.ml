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
            with | UnknownPrimitive(n) -> raise (Failure ("Unknown primitive: "^n))
                 | _ -> false) examples
        then 0.0
        else log 0.0)
  }



let keep_best_programs_in_frontier (k : int) (f : frontier) : frontier =
  {request = f.request;
   programs =  List.sort ~cmp:(fun (_,a) (_,b) -> if a > b then -1 else 1) f.programs |> flip List.take k }

(* Takes a frontier and a task. Ads in the likelihood on the task to
   the frontier and removes things that didn't hit the task *)
let score_programs_for_task (f:frontier) (t:task) : frontier =
  {request = f.request;
   programs = f.programs |> List.filter_map ~f:(fun (program, descriptionLength) ->
       let likelihood = t.log_likelihood program in
       if likelihood > -0.1 then 
         Some((program, descriptionLength +. likelihood))
       else None)
  }

let enumerate_solutions_for_tasks grammar tasks frontier_size ?keepTheBest:(keepTheBest = 0) : frontier list =
  (* what are all of the different types that we need to enumerate for *)
  let ts = List.map tasks ~f:(fun t -> t.task_type) |> List.dedup in
  
  (* the corresponding frontiers. [frontier] *)
  let fs = List.map ts ~f:(fun t -> iterative_deepening_enumeration grammar t frontier_size) in

  (* Construct the frontiers by keeping the programs that worked for each task *)
  let entire_frontiers =
    time_it "Evaluated programs against tasks"  @@ fun _ -> 
    List.map tasks ~f:(fun t ->
        let j = List.findi ts ~f:(fun _ tp -> tp = t.task_type) |> get_some |> fst in
        let frontier = List.nth fs j |> get_some in
        score_programs_for_task frontier t)
  in
  let best_frontiers = 
    match keepTheBest with
    | 0 -> entire_frontiers
    | k -> entire_frontiers |> List.map ~f:(keep_best_programs_in_frontier k)
  in
  List.iter2_exn best_frontiers tasks ~f:(fun f t ->
      match f.programs with
      | ((p,_)::_) -> 
        Printf.printf "HIT: %s with program %s\n" (t.name) (string_of_program p)
      | _ -> Printf.printf "MISS: %s\n" t.name);
  best_frontiers

