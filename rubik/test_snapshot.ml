
open Snapshot
open Graphics

let () =
  open_graph "";
  draw_image (make_image (take (start ()))) 0 0;
  ignore (wait_next_event [Button_down])
