type tree_cursor
type parser
type node
type tree

module Web_tree_sitter : sig
  type t

  val init : unit -> unit Fut.or_error
end

module Position : sig
  type t

  val row : t -> int
  val column : t -> int
end

module Language : sig
  type t

  val load : string -> t Fut.or_error
  val version : t -> int
  val field_count : t -> int
  val field_id_for_name : t -> string -> int
  val field_name_for_id : t -> int -> string
  val node_type_count : t -> int
end

module Node : sig
  type t = node

  val type_id : t -> int
  val type' : t -> string
  val end_position : t -> Position.t
  val end_index : t -> int
  val text : t -> string

  (* XXX returns int? *)
  val is_named : t -> bool
  val has_error : t -> bool
  val has_changes : t -> bool
  val is_missing : t -> bool
  val ( = ) : t -> t -> bool
  val child : t -> int -> t option
  val child_count : t -> int
  val named_child_count : t -> int
  val first_child : t -> t option
  val first_named_child : t -> t option
  val last_child : t -> t option
  val last_named_child : t -> t option
  val children : t -> t list
  val named_children : t -> t list
  val next_sibling : t -> t option
  val previous_sibling : t -> t option
  val next_named_sibling : t -> t option
  val previous_named_sibling : t -> t option
  val parent : t -> t option
  val walk : t -> tree_cursor
  val to_string : t -> string
end

module Tree : sig
  type t = tree

  val copy : t -> t
  val delete : t -> unit
  val root_node : t -> Node.t
  val get_language : t -> Language.t
  val walk : t -> tree_cursor
end

module Parser : sig
  type t = parser

  val new' : unit -> t
  val set_language : t -> Language.t -> unit
  val get_language : t -> Language.t
  val parse : t -> string -> Tree.t
  val reparse : t -> Tree.t -> string -> Tree.t
  val parse_structure : t -> f:(int -> int -> string) -> Tree.t
  val reparse_structure : t -> Tree.t -> f:(int -> int -> string) -> Tree.t
  val reset : t -> unit
  val set_timeout_micros : t -> int -> unit
  val get_timeout_micros : t -> int
end

module Tree_cursor : sig
  type t = tree_cursor

  val delete : t -> unit
  val reset : t -> Node.t -> unit
  val node_type : t -> string
  val node_type_id : t -> int
  val node_id : t -> int
  val node_is_named : t -> bool
  val node_is_missing : t -> bool
  val node_text : t -> string
  val start_position : t -> Position.t
  val end_position : t -> Position.t
  val start_index : t -> int
  val end_index : t -> int
  val current_node : t -> node
  val current_field_id : t -> int
  val current_field_name : t -> string option
  val goto_first_child : t -> bool
  val goto_next_sibling : t -> bool
  val goto_parent : t -> bool
end

module Query : sig
  type t

  val delete : t -> unit
  (* TODO: val matches : t -> node -> position -> position -> ? *)
  (* TODO: val captures : t -> node -> position -> position -> ? *)
end
