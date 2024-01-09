module Make (Ord : Map.OrderedType) : sig
  module Set : Set.S with type elt = Ord.t

  exception Cycle

  type mark = NotVisited | InProgress | Visited

  type t

  val empty : t
  (** The empty graph. *)

  val add_edge : t -> Ord.t -> Ord.t -> t
  (** [add_edge g n1 n2] adds an edge from [n1] to [n2] in [g]. *)

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
