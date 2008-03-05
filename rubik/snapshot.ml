(* File: snapshot.ml

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


(* At the beginning "gqcam --dump" was expected to deliver webcam
   snapshots.  However when using this command line snapshot facility,
   the colors did not come out well.  So vlc was chosen because the
   color adjustment is nice and it is cross platform. *)

(* According to http://forum.videolan.org/viewtopic.php?f=7&t=16492
   the snapshot command should be available in remote-control mode
   but "key" is not recognized in the current vlc version. *)
(* vlc -h *)
(* vlc --help --advanced *)
let vlc_remote =
  "vlc --extraintf rc v4l:// \
	--snapshot-path /tmp/ --snapshot-prefix ocaml --snapshot-format png"

(* This approach saves snapshots in a file that we can read in
   (execute "vlc -p image" for more info).  The "-I rc" starts the
   remote-control interface, so no display is shown and we have an
   easy way to quit.  *)
let vlc_remote =
  "vlc -I rc v4l:// -V image --image-out-replace --image-out-format png \
	--image-out-prefix ocaml"

(** Command : vlc dshow:// :dshow-adev="none" -I rc -V image --image-out-replace --image-out-format png --image-out-prefix ocaml **)
