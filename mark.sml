structure Mark :> sig
  eqtype t

  val unmarked : t

  val incr : unit -> unit

  val current : unit -> t
end = struct
  type t = int

  val unmarked = 0

  val r = ref 0

  fun incr () = r := !r + 1

  fun current () = !r
end

type mark = Mark.t
