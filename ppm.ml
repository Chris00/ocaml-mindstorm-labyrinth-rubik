(* File: ppm.ml

   Copyright (C) 2008

     Christophe Troestler <Christophe.Troestler@umh.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(* This library has benn written -- instead of using camlimages -- for
   windows portability. *)

(** Buffered read module with functions tailored to our needs *)
module Read =
struct
  (* Specialize [min] to integers for performance reasons (> 150% faster). *)
  let min x y = if (x:int) <= y then x else y

  let buffer_len = 4096

  type channel = {
    in_ch : in_channel;
    in_buf : string;   (* The data in the in_buf is at indexes i s.t. *)
    mutable in0 : int; (* in0 <= i < in1. *)
    mutable in1 : int; (* Invariant: 0 <= in0 ; in1 <= buffer_len
                          in1 < 0 indicates a closed channel. *)
  }

  let open_in fname = {
    in_ch = open_in_bin fname;
    in_buf = String.create buffer_len; in0 = 0; in1 = 0 }

  let close_in chan =
    if chan.in1 >= 0 then begin
      (* [chan] not yet closed *)
      close_in chan.in_ch;
      chan.in0 <- 0;
      chan.in1 <- -1
    end

  (* [fill_in_buf chan] refills in_buf if needed (when empty).  After
     this [in0 < in1] or [in1 = 0], the latter indicating that the end
     of file is reached (and then in0 = 0). *)
  let fill_in_buf chan =
    if chan.in0 >= chan.in1 then begin
      chan.in0 <- 0;
      chan.in1 <- input chan.in_ch chan.in_buf 0 buffer_len;
    end

  (* reads a single char. *)
  let char chan =
    fill_in_buf chan;
    if chan.in1 = 0 then raise End_of_file else begin
      let c = chan.in_buf.[chan.in0] in
      chan.in0 <- chan.in0 + 1;
      c
    end

  let unsafe_input chan buf ofs len =
    fill_in_buf chan;
    let r = min len (chan.in1 - chan.in0) in
    String.blit chan.in_buf chan.in0 buf ofs r;
    chan.in0 <- chan.in0 + r;
    r

  let rec fill_string chan s ofs len =
    if len > 0 then
      let r = unsafe_input chan s ofs len in
      if r = 0 then String.sub s 0 ofs
      else fill_string chan s (ofs+r) (len-r)
    else s

  (** Reads a string of [n] characters.  If there are less than [n]
      chars left in the channel, return these. *)
  let string chan len = fill_string chan (String.create len) 0 len

  (** Skips blanks, TABs, CRs, LFs.  If the end of file is reached, do
      nothing. *)
  let rec skip_spaces chan =
    fill_in_buf chan;
    if chan.in1 > 0 then begin
      let c = chan.in_buf.[chan.in0] in
      if c = ' ' || c = '\t' || c = '\n' || c = '\r' then begin
        chan.in0 <- chan.in0 + 1;
        skip_spaces chan
      end
    end

  let zero = Char.code '0'
  let rec gather_int chan n =
    fill_in_buf chan;
    if n < 0 then failwith "Ppm.uint: max_int exceeded";
    if chan.in1 = 0 then n              (* end of file *)
    else
      let c = chan.in_buf.[chan.in0] in
      if '0' <= c && c <= '9' then begin
        chan.in0 <- chan.in0 + 1;
        gather_int chan (n * 10 + Char.code c - zero)
      end
      else n                            (* next letter <> digit *)

  (** Reads an ASCII unsigned integer. *)
  let uint chan =
    fill_in_buf chan;
    if chan.in1 = 0 then raise End_of_file
    else
      let c = chan.in_buf.[chan.in0] in
      if c < '0' || c > '9' then failwith "Ppm.Read.uint"
      else gather_int chan 0

  (** Read a RGB color, each value being represented by 1 byte. *)
  let rgb1 chan =
    let r = Char.code(char chan) in     (* or End_of_file *)
    let g = Char.code(char chan) in
    let b = Char.code(char chan) in
    (r lsl 16) lor (g lsl 8) lor b

  let rgb2_color chan =
    let c1 = Char.code(char chan) in
    let _c0 = Char.code(char chan) in
    c1                                  (* least significant byte dropped *)

  (** Read a RGB color, each value being represented by 2 bytes. *)
  let rgb2 chan = 1
end

type color = int                        (* as Graphics *)
let transp = -1                         (* as Graphics *)

(* TODO: comments (strings starting whith '#') are not care about. *)
let as_matrix_exn fname =
  let fh = Read.open_in fname in
  if Read.string fh 2 <> "P6" then
    failwith "Ppm.as_matrix_exn: not a PPM file (not starting with P6)";
  Read.skip_spaces fh;
  let width = Read.uint fh in
  Read.skip_spaces fh;
  let height = Read.uint fh in
  Read.skip_spaces fh;
  let maxval = Read.uint fh in
  ignore(Read.string fh 1);             (* single white space *)
  let img = Array.create_matrix height width transp in
  let rgb =
    if maxval < 256 then (fun () -> Read.rgb1 fh)
    else (fun () -> Read.rgb2 fh) in
  for h = 0 to height - 1 do
    let row = img.(h) in
    for w = 0 to width - 1 do
      row.(w) <- rgb()
    done;
  done;
  Read.close_in fh;
  img
