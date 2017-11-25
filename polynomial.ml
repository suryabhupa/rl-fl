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
                      (* primitive "k2" tint 2; *)
                      (* primitive "k3" tint 3; *)
                      primitive "+" (tint @> tint @> tint) (+);
                      primitive "*" (tint @> tint @> tint) ( * );
                      (*                       primitive "apply" (t1 @> (t1 @> t0) @> t0) (fun x f -> f x); *)
                    ]


                                                             
let _ =
  let g = polynomial_grammar in
  let gf = fragment_grammar_of_grammar g in
  let gf = {logVariable = gf.logVariable;
            fragments = (FApply(FApply(FPrimitive(tint @> tint @> tint,"*"),FIndex(0)),FIndex(0)),
                         tint,
                         0.0)::gf.fragments} in
  let frontiers = enumerate_solutions_for_tasks g polynomial_tasks 50000 ~keepTheBest:1 in
  frontiers |> List.iter ~f:(fun frontier -> frontier.programs |> List.iter ~f:(fun (p,ll) ->
      let (fl,uses) = (likelihood_under_fragments gf (tint @> tint) p) in
      Printf.printf "%s %f %f %f\n" (string_of_program p) ll
        (likelihood_under_grammar g (tint @> tint) p)
        fl;
      show_uses gf uses));

  let fragments = propose_fragments_from_frontiers 0 frontiers
    (* List.map frontiers ~f:(fun f -> *)
    (*     List.map f.programs ~f:(fun (p,_) -> *)
    (*         propose_fragments 1 p)) *)
    (* |> List.concat  |> List.concat |> remove_duplicates *)
  in
   fragments |> List.iter ~f:(fun f ->
      Printf.printf "%s : %s\n" (string_of_fragment f) (infer_fragment_type f |> string_of_type))
   ;
   Printf.printf "Got %d fragments" (List.length fragments)
   ;
   induce_fragment_grammar fragments frontiers (fragment_grammar_of_grammar g)
