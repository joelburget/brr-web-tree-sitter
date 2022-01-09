type edit
type language
type node
type parser
type parser_options
type position
type query
type range
type tree
type tree_cursor

module Web_tree_sitter : sig
  type t

  val init : unit -> unit Fut.or_error
end

module Position : sig
  type t = position

  val row : t -> int
  val column : t -> int
end

module Range : sig
  type t = range

  val start_index : t -> int
  val end_index : t -> int
  val start_position : t -> position
  val end_position : t -> position
end

module Edit : sig
  type t = edit

  val new'
    :  start_index:int
    -> old_end_index:int
    -> new_end_index:int
    -> start_position:position
    -> old_end_position:position
    -> new_end_position:position
    -> t

  val start_index : t -> int
  val old_end_index : t -> int
  val new_end_index : t -> int
  val start_position : t -> position
  val old_end_position : t -> position
  val new_end_position : t -> position
end

module Parser : sig
  type t = parser
  type input = start_index:int -> ?start_point:position -> ?end_index:int -> string option

  module Options : sig
    type t = parser_options

    val new' : included_ranges:range list -> t
  end

  (** Create a new parser. *)
  val new' : unit -> t

  (** Set the language that the parser should use for parsing.

      Returns a boolean indicating whether or not the language was successfully assigned.
      True means assignment succeeded. False means there was a version mismatch: the
      language was generated with an incompatible version of the Tree-sitter CLI. Check
      the language's version using [language_version] and compare it to this library's
      [TREE_SITTER_LANGUAGE_VERSION] and [TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION]
      constants. *)
  val set_language : t -> language -> unit

  (** Get the parser's current language. *)
  val get_language : t -> language

  (* TODO: add options *)

  (** Use the parser to parse some source code and create a syntax tree.

      This function returns `Some` syntax tree on success, and [None] on failure. There
      are two possible reasons for failure:

      + The parser does not have a language assigned. Check for this using the
        [parser_language] function.
      + Parsing was cancelled due to a timeout that was set by an earlier call to the
        [parser_set_timeout_micros] function. You can resume parsing from where the parser
        left out by calling [parse] again with the same arguments. Or you can start
        parsing from scratch by first calling [reset]. *)
  val parse : t -> ?options:Options.t -> string -> tree

  (** See [parse] *)
  val reparse : t -> ?options:Options.t -> tree -> string -> tree

  (** See [parse] *)
  val parse_structure : t -> ?options:Options.t -> input -> tree

  (** See [parse] *)
  val reparse_structure : t -> ?options:Options.t -> tree -> input -> tree

  (** Instruct the parser to start the next parse from the beginning.

      If the parser previously failed because of a timeout or a cancellation, then by
      default, it will resume where it left off on the next call to [parse] or other
      parsing functions. If you don't want to resume, and instead intend to use this
      parser to parse some other document, you must call [reset] first. *)
  val reset : t -> unit

  (** Set the maximum duration in microseconds that parsing should be allowed to take
      before halting.

      If parsing takes longer than this, it will halt early, returning NULL. See [parse]
      for more information. *)
  val set_timeout_micros : t -> int -> unit

  (** Get the duration in microseconds that parsing is allowed to take. *)
  val get_timeout_micros : t -> int
end

module Tree : sig
  type t = tree

  (** Create a shallow copy of the syntax tree. This is very fast.

      You need to copy a syntax tree in order to use it on more than one thread at a time,
      as syntax trees are not thread safe. *)
  val copy : t -> t

  (** Delete the syntax tree, freeing all of the memory that it used. *)
  val delete : t -> unit

  (** Get the root node of the syntax tree. *)
  val root_node : t -> node

  (** Get the language that was used to parse the syntax tree. *)
  val get_language : t -> language

  val walk : t -> tree_cursor

  (** Edit the syntax tree to keep it in sync with source code that has been edited.

      You must describe the edit both in terms of byte offsets and in terms of (row,
      column) coordinates. *)
  val edit : t -> edit -> unit

  (** Compare an old edited syntax tree to a new syntax tree representing the same
      document, returning an array of ranges whose syntactic structure has changed.

      For this to work correctly, the old syntax tree must have been edited such that its
      ranges match up to the new tree. Generally, you'll want to call this function right
      after calling one of the [parse] functions. You need to pass the old tree that was
      passed to parse, as well as the new tree that was returned from that function. *)
  val get_changed_ranges : t -> t -> range
end

module Node : sig
  type t = node

  (** Get the node's type as a string. *)
  val type' : t -> string

  (** Get the node's type as a numerical id. *)
  val type_id : t -> int

  val end_position : t -> position
  val end_index : t -> int
  val text : t -> string

  (** Check if the node is {e named}. Named nodes correspond to named rules in the
      grammar, whereas {e anonymous} nodes correspond to string literals in the grammar. *)
  val is_named : t -> bool

  (** Check if the node is {e missing}. Missing nodes are inserted by the parser in *
      order to recover from certain kinds of syntax errors. *)
  val is_missing : t -> bool

  (** Check if the node is a syntax error or contains any syntax errors. *)
  val has_error : t -> bool

  val has_changes : t -> bool
  val ( = ) : t -> t -> bool

  (** Get the node's child at the given index, where zero represents the first child. *)
  val child : t -> int -> t option

  (** Get the node's number of children. *)
  val child_count : t -> int

  (** Get the node's number of {e named} children. *)
  val named_child_count : t -> int

  val first_child : t -> t option
  val first_named_child : t -> t option
  val last_child : t -> t option
  val last_named_child : t -> t option

  (** Get the node's children. *)
  val children : t -> t list

  (** Get the node's {e named} children. *)
  val named_children : t -> t list

  (** Get the node's next / previous sibling. *)
  val next_sibling : t -> t option

  val previous_sibling : t -> t option

  (** Get the node's next / previous {e named} sibling. *)
  val next_named_sibling : t -> t option

  val previous_named_sibling : t -> t option

  (** Get the node's immediate parent. *)
  val parent : t -> t option

  (* TODO val descendant_for_index : int -> node *)

  val walk : t -> tree_cursor

  (** Get an S-expression representing the node as a string. *)
  val to_string : t -> string
end

module Tree_cursor : sig
  type t = tree_cursor

  (** Delete a tree cursor. *)
  val delete : t -> unit

  (** Re-initialize a tree cursor to start at a different node. *)
  val reset : t -> node -> unit

  (** Get the tree cursor's current node. *)
  val current_node : t -> node

  val node_type : t -> string
  val node_type_id : t -> int
  val node_id : t -> int
  val node_is_named : t -> bool
  val node_is_missing : t -> bool
  val node_text : t -> string
  val start_position : t -> position
  val end_position : t -> position
  val start_index : t -> int
  val end_index : t -> int

  (** Get the field id of the tree cursor's current node.

      This returns zero if the current node doesn't have a field. *)
  val current_field_id : t -> int

  (** Get the field name of the tree cursor's current node. *)
  val current_field_name : t -> string option

  (** Move the cursor to the first child of its current node.

      This returns [true] if the cursor successfully moved, and returns [false] if there
      were no children. *)
  val goto_first_child : t -> bool

  (** Move the cursor to the next sibling of its current node.

      This returns [true] if the cursor successfully moved, and returns [false] if there
      was no next sibling node. *)
  val goto_next_sibling : t -> bool

  (** Move the cursor to the parent of its current node.

      This returns [true] if the cursor successfully moved, and returns [false] if there
      was no parent node (the cursor was already on the root node). *)
  val goto_parent : t -> bool
end

module Query : sig
  type t

  module Capture : sig
    type t =
      { name : string
      ; text : string
      }
  end

  module Match : sig
    type t =
      { pattern : int
      ; captures : Capture.t list
      }
  end

  module Operand : sig
    type t =
      { type' : string
      ; name : string
      }
  end

  module Predicate : sig
    type t =
      { operator : string
      ; operands : Operand.t list
      }
  end

  (** Delete a query. *)
  val delete : t -> unit

  val matches : t -> node -> position -> position -> Match.t list
  val captures : t -> node -> position -> position -> Capture.t list
  val predicates_for_pattern : t -> int -> Predicate.t list
  val did_exceed_match_limit : t -> bool
end

module Language : sig
  type t = language

  val load : string -> t Fut.or_error

  (** Get the ABI version number for this language. This version number is used to ensure
      that languages were generated by a compatible version of Tree-sitter. *)
  val version : t -> int

  (** Get the number of distinct field names in the language. *)
  val field_count : t -> int

  (** Get the numerical id for the given field name string. *)
  val field_id_for_name : t -> string -> int

  (** Get the field name string for the given numerical id. *)
  val field_name_for_id : t -> int -> string

  val id_for_node_type : t -> string -> bool -> string option

  (** Get the number of distinct node types in the language. *)
  val node_type_count : t -> int

  val node_type_for_id : t -> int -> string option
  val node_type_is_named : t -> int -> bool
  val node_type_is_visible : t -> int -> bool
  val query : t -> string -> query
end
