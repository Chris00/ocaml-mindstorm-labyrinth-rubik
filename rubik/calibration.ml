open Graphics
open Init_color
open Printf

let calibrate_webcam () =
  let wc = Snapshot.start () in
  let img = Snapshot.take wc in
  let height = Array.length img
  and width = Array.length img.(0) in
  open_graph (sprintf " %ix%i"  width height);
  set_color (rgb 0 255 242);
  print_endline "press a key when the calibration is done!";
  let fill_checking_zone x y =
    let col = Pick.average( Pick.pick_point img (Pick.abs x) (Pick.ord y)) in

    (* for checking the fiability of the detection color module *)
    printf "%s " (Color.to_string (Color.name col));

    let (a,b,c) = col in
    set_color (rgb a b c);
    fill_rect (Pick.abs x) (Pick.ord y) 14 14;
    set_color (rgb 0 255 242);
    draw_rect (Pick.abs x) (Pick.ord y) 14 14 in
  (* refreshing the snapshot cubie.*)
  let rec refresh () =
    (* take a new snapshot *)
    let img = Snapshot.take wc in
    draw_image (make_image img) 0 0;
    Array.iter (fun x ->


                  (* for checking the fiability of the detection color module *)
                  printf "\n!";

                  Array.iter (fun y -> fill_checking_zone x y) [|0;1;2|]
               ) [|0;1;2|];
    Unix.sleep 2;
    if not (key_pressed ()) then refresh ()
  in refresh ();
  close_graph ()

let calibrate_mechanics _ =
  ()

let () =
  calibrate_webcam ()
