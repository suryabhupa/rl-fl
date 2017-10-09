open Core.Std

open Utils
open Type
open Program
open Enumeration
open Task
open Grammar


let polynomial_grammar =
  primitive_grammar [ primitive "k0" tint 0;
                      primitive "k1" tint 1;
                      primitive "+" (tint @> tint @> tint) (+);
                      primitive "*" (tint @> tint @> tint) ( * );
                      primitive "let" (t1 @> (t1 @> t0) @> t0) (fun x f -> f x);
                    ]


                                                             
let _ =
  let g = polynomial_grammar in
  iterative_deepening_enumeration polynomial_grammar (tint @> tint) 1000 |>
  List.iter ~f:(fun p ->
      Printf.printf "%s has likelihood %f\n" (show_program p) (likelihood_under_grammar g (tint @> tint) p))
