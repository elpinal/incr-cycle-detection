structure Level :> sig
  eqtype t

  val initial : t

  val < : t * t -> bool

  val incr : t -> t

  val to_int : t -> int
end = struct
  type t = int

  val initial = 1

  val op< = op<

  fun incr x = x + 1

  fun to_int x = x
end

type level = Level.t
