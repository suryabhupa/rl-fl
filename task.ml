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


        
let score_programs_for_tasks programs tasks =
  List.map tasks ~f:(fun t ->
      let ss = List.map programs t.log_likelihood in
      if List.mem ss 0.0 then Printf.printf "HIT: %s\n" t.name else ();
      ss)

let get_solutions_for_tasks programs tasks : program list list =
  let scores = score_programs_for_tasks programs tasks in
  List.map scores ~f:(fun likelihoods ->
      List.zip_exn  programs likelihoods |> List.filter_map ~f:(fun (p,l) ->
        if l = 0.0 then Some(p) else None))
        
let enumerate_solutions_for_tasks grammar tasks frontier_size : program list list =
  (* what are all of the different types that we need to enumerate for *)
  let ts = List.map tasks ~f:(fun t -> t.task_type) |> List.dedup in
  (* the corresponding frontiers *)
  let fs = List.map ts ~f:(fun t -> iterative_deepening_enumeration grammar t frontier_size) in
  List.map tasks ~f:(fun t ->
      let j = List.findi ts ~f:(fun _ tp -> tp = t.task_type) |> get_some |> fst in
      let frontier = List.nth fs j |> get_some in
      get_solutions_for_tasks frontier [t] |> List.hd |> get_some)

let top_solutions_for_tasks k grammar tasks solutions =
  let top_solution_for_task t s =
    List.map s ~f:(fun p -> (likelihood_under_grammar grammar t.task_type p, p))
    |> List.sort ~cmp:(fun (l1,_) (l2,_) -> if l2 -. l1 > 0.0 then 1 else -1)
    |> flip List.take k
    |> List.map ~f:(fun (_,p) -> p)
  in List.map2_exn ~f:top_solution_for_task tasks solutions


let polynomial_tasks =
  (0--9) |> List.map ~f:(fun a ->
      (0--9) |> List.map ~f:(fun b ->
          (0--9) |> List.map ~f:(fun c ->
              let examples = List.map (0--5) ~f:(fun x -> (x, a*x*x + b*x + c)) in
              let n = Printf.sprintf "(%i x^2 + %i x + %i)" a b c in
              supervised_task n (tint @> tint) examples)))
  |> List.concat |> List.concat


let arithmetic_grammar =
  primitive_grammar [ primitive "k0" tint 0;
                      primitive "k1" tint 1;
                      primitive "+" (tint @> tint @> tint) (+);
                      primitive "*" (tint @> tint @> tint) ( * );
                      primitive "apply" (t0 @> (t0 @> t1) @> t1) (fun x f -> f x);]





