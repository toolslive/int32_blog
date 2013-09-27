open Ocamlbuild_plugin;;
dispatch begin function
 | After_rules ->
     dep ["ocaml"; "link"; "use_zooph"]["zooph.o"];

     flag ["ocaml";"link";"use_zooph";"byte"] (S[A"-custom"]);
     flag ["c";"compile"] (S[A"-ccopt";A"-O3";]);
     flag ["c";"use_zooph"; "ocamlmklib"] & S[A"-ccopt"; A"-I."];
     flag ["ocaml";"use_zooph";"link";"library";"byte"] &
       S[A"-dllib";A"-lzooph"];
 | _ -> ()
 end
