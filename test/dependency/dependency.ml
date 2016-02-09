open Soup

(* For pre-4.01 distributions of OCaml. *)
let (|>) x f = f x

let () =
  let text = "<p></p>" in
  if text |> parse |> to_string <> text then
    exit 1
  else ()
