(* File: translator.mli *)

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
              val cog_is_set_left : bool
                (** True if cogs are placed to turn platform in clockwise
                    False if cogs are placed to turn plaftform in clockwis. *)
            end):

sig

  val make : Rubik.generator*int -> unit
    (** make[g r] Create the physical movement associeted to the
        movement (g*r). (See rubik)*)

  val face_iter : (Rubik.generator -> int -> unit) -> unit
    (** Put succecively each face of the cube on top to take a snapshot.
        face_iter [g i] ask to take a snapshot of the face g, which is in the
        position i (See the documentation of init_color.ml).
        Here we choise the letters associates to faces (see the documentation
        of the fonction 'transform_gen')so that the initial state is correct
        after all snapshot.*)
end
