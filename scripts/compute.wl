#!/usr/bin/env wolframscript
(* Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl> *)

(* Given a command-line parameter h approximates *)
(* the value L_0(rho) where rho is the dominating *)
(* singularity corresponding to the generating function *)
(* for plain lambda terms. *)

h = ToExpression[$ScriptCommandLine[[2]]];
Subscript[L,h][z_] = (1-z-Sqrt[(1-z)^2 - (4z^2(1-z))/(1-z)])/(2z);
Do[Subscript[L,i][z_] = (1-Sqrt[1-4z(z Subscript[L,i+1][z] + (z(1-z^i))/(1-z))])/(2z), {i,h-1,0,-1}];

rho = Solve[(1-z)^3-4z^2==0,z][[3]][[1]][[2]];
val = Subscript[L,0][rho];
Print[N[val]]
