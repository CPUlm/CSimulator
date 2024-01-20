module Make (Ord : Map.OrderedType) : sig
  module Set : Set.S with type elt = Ord.t

  exception Cycle

  type mark = NotVisited | InProgress | Visited

  type t

  type mut

  val empty : unit -> mut
  (** The empty graph. *)

  val add_node : mut -> Ord.t -> unit
  (** [add_node g n] adds the node [n] to the graph in [g]. *)

  val add_edge : mut -> Ord.t -> Ord.t -> unit
  (** [add_edge g n1 n2] adds an edge from [n1] to [n2] in [g]. *)

  val freeze : mut -> t
  (** [freeze g] freeze the graph [g] and returns an immutable structure. *)

  val topological : t -> (Ord.t, int) Hashtbl.t
  (** [topological] returns a topological ordering over the vertex of the graph [g].*)

  val children : t -> Ord.t -> Set.t
  (** [children g v] returns the childrens of [v] in [g]. *)

  val parents : t -> Ord.t -> Set.t
  (** [parents g v] returns the parents of [v] in [g]. *)

  val iter : (Ord.t -> Set.t -> Set.t -> unit) -> t -> unit
  (** [iter f g] calls [f] for each vertex of the graph with the node,
      its parents and its childrens. *)
end
