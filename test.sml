structure C = Cycle (struct type t = int end)

open C

val (g, [v, w, x]) = make_vertices [2, 3, 888]

val () = add_edge g w v
val () = add_edge g w x
val () = add_edge g v x

val xs = sort g

val () = app (fn a => print (Int.toString a ^ "\n")) xs
