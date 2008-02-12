(* File : solver.mli *)

module Make(C: sig
  val conn : 'a Mindstorm.conn
  val motor_left : Mindstorm.Motor.port
  val motor_right : Mindstorm.Motor.port
  val motor_ultra : Mindstorm.Motor.port
  val light_port : Mindstorm.Sensor.port
  val ultra_port : Mindstorm.Sensor.port
end) :
sig

val next_case_to_explore : Labyrinth.Coord.t  -> Labyrinth.dir_rel list

type cont = unit -> unit

val look_walls : cont  -> unit

val follow_path: cont -> Labyrinth.dir_rel list -> unit

val go_left : cont -> unit

val go_right : cont -> unit

val go_straight : cont -> unit

val go_back : cont -> unit

end
