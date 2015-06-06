open Ocamlbuild_plugin;;

dep ["link"; "ocaml"; "codeloader_stubs"] ["codeloader_stubs.o"];;
mark_tag_used "codeloader_stubs";
