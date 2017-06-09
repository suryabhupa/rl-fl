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
        if List.for_all ~f:(fun (x,y) ->
            try
              f x = y
            with _ -> false) examples
        then 0.0
        else log 0.0)
  }

let load_list_tasks f =
  let open Yojson.Basic.Util in
  let j = Yojson.Basic.from_file f in
  j |> to_list |> List.map ~f:(fun t ->
      Printf.printf "%s\n" (Yojson.Basic.pretty_to_string t);
      let name = t |> member "name" |> to_string in
      Printf.printf "%s\n" name;
      (* change if needed *)
      let returnType = ref tint in
      let ex =
        t |> member "examples" |> to_list |>
         List.map ~f:(fun example ->
            let [x;y] = to_list example in
            (x |> to_list |> List.map ~f:to_int,
             try
               y |> to_int |> magical
             with _ -> 
               begin
                  returnType := tlist tint;
                  y |> to_list |> List.map ~f:to_int |> magical
                end))
      in
      supervised_task name (tlist tint @> !returnType) ex)
        
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
                      primitive "-" (tint @> tint @> tint) (-);
                      primitive "sort" (tlist tint @> tlist tint) (List.sort ~cmp:(fun x y -> x - y));
                      primitive "reverse" (tlist tint @> tlist tint) (List.rev);
                      primitive "append"  (tlist tint @> tlist tint @> tlist tint) (@);
                      primitive "singleton"  (tint @> tlist tint) (fun x -> [x]);
                      primitive "slice" (tint @> tint @> tlist tint @> tlist tint) (slice);
                      primitive "length" (tlist tint @> tint) (List.length);
                      primitive "map" ((tint @> tint) @> (tlist tint) @> (tlist tint)) (fun f l -> List.map ~f:f l);
                      primitive "filter" ((tint @> tboolean) @> (tlist tint) @> (tlist tint)) (fun f l -> List.filter ~f:f l);
                      primitive "eq?" (tint @> tint @> tboolean) ( = );
                      primitive "apply" (t0 @> (t0 @> t1) @> t1) (fun x f -> f x);]

let _ =
  (* change these to the grammar and appropriate type as needed *)
  let grammar = list_grammar
  and tasks = load_list_tasks "list_tasks.json"
  in
  let solutions = enumerate_solutions_for_tasks grammar tasks 100000 in


  List.iter2_exn tasks solutions ~f:(fun t s ->
      if s = [] then Printf.printf "No solution for task %s\n" t.name
      else Printf.printf "A solution for task %s:\n\t%s\n" t.name (show_program @@ List.hd_exn s))
  
