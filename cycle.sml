functor Cycle (X : sig type t end) :> sig
  type graph
  type vertex

  exception SelfRef of vertex * vertex
  exception Cycle   of vertex * vertex

  val make_vertices : X.t list -> graph * vertex list

  val add_edge : graph -> vertex -> vertex -> unit
end = struct
  type vertex = int

  type cell =
    { content : X.t
    , level : level ref
    , mark : mark ref
    , incoming : vertex list ref
    , outgoing : vertex list ref
  }

  type graph = cell vector

  datatype backward_result
    = Interrupted
    | NotFoundInTheSameLevel

  exception SelfRef of vertex * vertex
  exception Cycle   of vertex * vertex

  fun make_cell c =
    { content = c
    , level = ref Level.initial
    , mark = ref Mark.unmarked
    , incoming = ref []
    , outgoing = ref []
    }

  fun make_vertices xs =
  let
    val vec = Vector.fromList (map make_cell xs)
  in
    (vec, List.tabulate (Vector.length vec, fn x => x))
  end

  fun get g v : cell =
    Vector.sub (g, v)

  fun add_out (c : cell) (v : vertex) : unit =
    #outgoing c := v :: !(#outgoing c)

  fun add_in (c : cell) (v : vertex) : unit =
    #incoming c := v :: !(#incoming c)

  fun get_level (c : cell) : Level.t =
    !(#level c)

  fun backward (fuel : int ref) g v w : backward_result =
  let
    fun go [] = NotFoundInTheSameLevel
      | go (u :: us) =
          if !fuel = 0
          then Interrupted
          else if u = w
          then raise Cycle(w, v)
          else
            ( #mark (get g u) := Mark.current ()
            ; fuel := !fuel - 1
            ; case backward fuel g u w of
                   NotFoundInTheSameLevel => go us
                 | Interrupted            => Interrupted
            )
  in
    go (!(#incoming (get g v)))
  end

  fun forward g w : unit =
  let
    fun go [] = ()
      | go (u :: us) =
          let
            val c1 : cell = get g w
            val c2 : cell = get g u
            val () =
              if !(#mark c2) = Mark.current ()
              then raise Cycle(w, u)
              else ()
            open Level
          in
            if get_level c1 < get_level c2
            then go us
            else if get_level c1 = get_level c2
            then (add_in c2 w; go us)
            else
              ( #level c2 := get_level c1
              ; #incoming c2 := [w]
              ; forward g u
              ; go us
              )
          end
  in
    go (!(#outgoing (get g w)))
  end

  exception Success

  fun add_edge g v w =
  let
    val () =
      if v = w
      then raise SelfRef(v, w)
      else ()

    val c1 = get g v
    val c2 = get g w
    open Level

    val () =
      if get_level c1 < get_level c2
      then (add_out c1 w; raise Success)
      else ()

    val () = Mark.incr ()
    val () = #mark c1 := Mark.current ()
    val fuel = ref (Level.to_int (!(#level c1)))
    val bres = backward fuel g v w

    val () =
      case bres of
           Interrupted =>
             ( #level c2 := Level.incr (get_level c1)
             ; #incoming c2 := []
             )
         | NotFoundInTheSameLevel =>
             if get_level c1 = get_level c2
             then
               ( add_out c1 w
               ; add_in c2 v
               ; raise Success
               )
             else
               ( #level c2 := get_level c1
               ; #incoming c2 := []
               )

    val () = forward g w
  in
    add_out c1 w;
    if get_level c1 = get_level c2
    then add_in c2 v
    else ()
  end handle Success => ()
end
