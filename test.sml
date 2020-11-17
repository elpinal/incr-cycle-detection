structure C = Cycle (struct type t = int end)

open C

val (g, [v, w, x]) = make_vertices [2, 3, 888]

val () = add_edge g v w
val () = add_edge g w x
val () = add_edge g x v
