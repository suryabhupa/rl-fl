open Core.Std

open Utils
open Type
open Program
open Enumeration
open Task
open Grammar
open Compression


let maximumCoefficient = 3
  
let polynomial_tasks =
  (0--maximumCoefficient) |> List.map ~f:(fun a ->
      (0--maximumCoefficient) |> List.map ~f:(fun b ->
          (0--maximumCoefficient) |> List.map ~f:(fun c ->
              let examples = List.map (0--5) ~f:(fun x -> (x, a*x*x + b*x + c)) in
              let n = Printf.sprintf "(%i x^2 + %i x + %i)" a b c in
              supervised_task n (tint @> tint) examples)))
  |> List.concat |> List.concat

let polynomial_grammar =
  primitive_grammar [ primitive "k0" tint 0;
                      primitive "k1" tint 1;
                      primitive "+" (tint @> tint @> tint) (+);
                      primitive "*" (tint @> tint @> tint) ( * );
                      primitive "let" (t1 @> (t1 @> t0) @> t0) (fun x f -> f x);
                    ]


                                                             
let _ =
  let g = polynomial_grammar in
  let frontiers = enumerate_solutions_for_tasks g polynomial_tasks 1000 ~keepTheBest:10 in
  let fragments = propose_fragments_from_frontiers 1 frontiers
    (* List.map frontiers ~f:(fun f -> *)
    (*     List.map f.programs ~f:(fun (p,_) -> *)
    (*         propose_fragments 1 p)) *)
    (* |> List.concat  |> List.concat |> remove_duplicates *)
  in
   fragments |> List.iter ~f:(fun f ->
      Printf.printf "%s\n" (string_of_fragment f))
   ;
   Printf.printf "Got %d fragments" (List.length fragments)
   ;
   induce_fragments fragments frontiers
