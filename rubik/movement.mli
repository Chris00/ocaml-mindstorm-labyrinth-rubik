(* File: rubik_solver.mli *)

module Make(C: sig
              val conn : Mindstorm.bluetooth Mindstorm.conn
                (** The bluetooth connection of the robot. *)
              val motor_fighter : Mindstorm.Motor.port
                (** The port of the fighther motor. *)
              val motor_hand : Mindstorm.Motor.port
                (** The port of the hand motor. *)
              val motor_pf : Mindstorm.Motor.port
                (** The port of the platform motor. *)
              val push_hand_port : Mindstorm.Sensor.port
                (** The port of the hand switch. *)
              val push_fighter_port : Mindstorm.Sensor.port
                (** The port of the fighter switch. *)
            end):
sig

  val stop : unit  -> unit
    (** Stop the event loop when the rubik is solved. *)

  val run_loop : unit -> unit
    (** Launch the robot. *)

  val kick : (unit -> unit)-> unit
    (** Kick the rubik's cube to turn him of quart. *)

  val turn_pf : int  -> (unit -> unit) -> unit
    (** [turn_pf t k] turn the platform of t quaters of turn
        (if t is negative, it turn in the other way round. *)

  val turn_rubik_left : (unit -> unit)-> unit
    (** Turn the 3th line of the cube of 90 degrees in clockwize. *)

  val turn_rubik_right : (unit -> unit) -> unit
    (** Turn the 3th line of the cube of 90 degrees in opposite clockwize*)

  val turn_rubik_half : (unit -> unit) -> unit
    (** Turn the 3th line of the cube of half turn *)

  val initialize : (unit -> unit) -> unit
    (** Give a good position to the robot for it to begin its operations *)
end
