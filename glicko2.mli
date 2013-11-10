(* glicko2 ranks contestants according to the Glicko 2 ranking system *)

type player =
	{ r : float;
	  rd : float;
	  sigma : float }

type opponent =
	{ rj : float;
	  rdj : float;
	  sj : float }

val pi : float
val square : float -> float
val g : float -> float
val e : float -> float -> float -> float
val scale : float -> float -> float * float

val rate : player -> opponent list -> player

val bench_data : unit -> (player * opponent list)
val bench : (player * opponent list) -> unit
