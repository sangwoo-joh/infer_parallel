include Core

(* Compare policy: generic compare mostly disabled *)
let compare = No_polymorphic_compare.compare

let equal = No_polymorphic_compare.equal

let ( = ) = No_polymorphic_compare.( = )

module ANSITerminal : module type of ANSITerminal = struct
  include ANSITerminal

  (* more careful about when the channel is connected to a tty *)

  let print_string = if Unix.(isatty stdout) then print_string else fun _ -> Stdlib.print_string

  let prerr_string = if Unix.(isatty stderr) then prerr_string else fun _ -> Stdlib.prerr_string

  let printf styles fmt = Format.ksprintf (fun s -> print_string styles s) fmt

  let eprintf styles fmt = Format.ksprintf (fun s -> prerr_string styles s) fmt

  let sprintf = if Unix.(isatty stderr) then sprintf else fun _ -> Printf.sprintf
end
