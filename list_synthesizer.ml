open Core.Std

open Utils
open Type
open Program
open Enumeration
open Task

let load_list_tasks f =
  let open Yojson.Basic.Util in
  let j = Yojson.Basic.from_file f in
  j |> to_list |> List.map ~f:(fun t ->
      (*       Printf.printf "%s\n" (Yojson.Basic.pretty_to_string t); *)
      let name = t |> member "name" |> to_string in
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
  
