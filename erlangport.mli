
val read : in_channel -> Eterm.t
val write : out_channel -> Eterm.t -> unit

val interact : (Eterm.t -> Eterm.t) -> unit
                                         
