open Graphics
open Init_color
open Printf

let calibrate_webcam () =
  (* let wc = Snapshot.start () in *)
  (*let img = Snapshot.take wc in *)
  let img = Ppm.as_matrix_exn "/Users/marku/Desktop/left.ppm" in
  let height = Array.length img
  and width = Array.length img.(0) in
  open_graph (sprintf " %ix%i"  width height);
  set_color (rgb 0 255 242);
  print_endline "press a key when the calibration is done!";
  let fill_checking_zone x y =
    let c = Pick.average( Pick.pick_point img (Pick.abs x) (Pick.ord y)) in
    printf "%s " (Color.to_string (Color.name c));
    let (a,b,c) = c in
    set_color (rgb a b c);
    fill_rect (Pick.abs x) (Pick.ord y) 14 14;
    set_color (rgb 0 255 242);
    draw_rect (Pick.abs x) (Pick.ord y) 14 14 in
  (* refreshing the snapshot cubie.*)
  let rec refresh () =
    (* take a new snapshot *)
    (* let img = Snapshot.take wc in *)
    let img = Ppm.as_matrix_exn "/Users/marku/Desktop/left.ppm" in
    draw_image (make_image img) 0 0;
    Array.iter (fun x ->
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
