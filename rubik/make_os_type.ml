(* File: make_os_type.ml

   Copyright (C) 2008

   Christophe Troestler <chris_77@users.sourceforge.net>
   WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


(** Return the platform on stdout for conditional statements in the
    Makefile. *)

#load "unix.cma";;

let () =
  match Sys.os_type with
  | "Win32" | "Cygwin" -> print_string "OS=-DWIN32\n"
  | _ ->
      (* Distinguish between Unix and MacOS using uname *)
      let fh = Unix.open_process_in "uname -s" in
      let name = input_line fh in
      let os = if name = "Darwin" then "MACOS" else "UNIX" in
      let status = Unix.close_process_in fh in
      print_string("OS=-D" ^ os ^ "\n");
      exit(match status with
           | Unix.WEXITED e -> e
           | _ -> -1)


(* Local Variables: *)
(* compile-command: "make -k make_os_type.cmo" *)
(* End: *)
