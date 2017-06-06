open Core.Std

open Utils
open Type
open Program
open Enumeration



type task =
  { name: string; task_type: tp;
    log_likelihood: program -> float;
  }

let supervised_task name ty examples =
  { name = name;
    task_type = ty;
    log_likelihood = (fun p ->
        let f = evaluate [] p in
        if List.for_all ~f:(fun (x,y) -> f x = y) examples
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



let list_grammar =
  primitive_grammar [ primitive "k0" tint 0;
                      primitive "k1" tint 1;
                      primitive "+" (tint @> tint @> tint) (+);
                      primitive "map" ((tint @> tint) @> (tlist tint) @> (tlist tint)) (fun f l -> List.map ~f:f l);
                      primitive "apply" (t0 @> (t0 @> t1) @> t1) (fun x f -> f x);]

let list_tasks =
  [supervised_task "map +1" ((tlist tint) @> (tlist tint))
     [([1;2;3], [2;3;4]);
      ([9],     [10]);
      ([],      [])]
  ]

let _ =
  (* change these to the grammar and appropriate type as needed *)
  let grammar = list_grammar
  and tp = ((tlist tint) @> (tlist tint)) (* tint @> tint *)
  and tasks = list_tasks  
  in
  let programs = iterative_deepening_enumeration grammar tp 1000 in
  let solutions = get_solutions_for_tasks programs tasks in


  List.iter2_exn tasks solutions ~f:(fun t s ->
      if s = [] then Printf.printf "No solution for task %s\n" t.name
      else Printf.printf "A solution for task %s:\n\t%s\n" t.name (show_program @@ List.hd_exn s))
  
