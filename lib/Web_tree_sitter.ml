type edit = Jv.t
type language = Jv.t
type node = Jv.t
type parser = Jv.t
type parser_options = Jv.t
type position = Jv.t
type query = Jv.t
type range = Jv.t
type tree = Jv.t
type tree_cursor = Jv.t

let tree_sitter () = Jv.get Jv.global "__TreeSitter"
let id x = x
let to_option = Jv.to_option id
let to_list = Jv.to_list id

module Position = struct
  type t = position

  let row t = Jv.get t "row" |> Jv.to_int
  let column t = Jv.get t "column" |> Jv.to_int
end

module Range = struct
  type t = range

  let start_index t = Jv.get t "startIndex" |> Jv.to_int
  let end_index t = Jv.get t "endIndex" |> Jv.to_int
  let start_position t = Jv.get t "startPosition"
  let end_position t = Jv.get t "endPosition"
end

module Edit = struct
  type t = edit

  let new'
      ~start_index
      ~old_end_index
      ~new_end_index
      ~start_position
      ~old_end_position
      ~new_end_position
    =
    Jv.obj
      [| "startIndex", Jv.of_int start_index
       ; "oldEndIndex", Jv.of_int old_end_index
       ; "newEndIndex", Jv.of_int new_end_index
       ; "startPosition", start_position
       ; "oldEndPosition", old_end_position
       ; "newEndPosition", new_end_position
      |]
  ;;

  let start_index t = Jv.get t "startIndex" |> Jv.to_int
  let old_end_index t = Jv.get t "oldEndIndex" |> Jv.to_int
  let new_end_index t = Jv.get t "newEndIndex" |> Jv.to_int
  let start_position t = Jv.get t "startPosition"
  let old_end_position t = Jv.get t "oldEndPosition"
  let new_end_position t = Jv.get t "newEndPosition"
end

module Web_tree_sitter = struct
  type t = Jv.t

  let init () = Jv.call (tree_sitter ()) "init" [||] |> Fut.of_promise ~ok:ignore
end

module Language = struct
  type t = language

  let load wasm =
    Jv.call (Jv.get (tree_sitter ()) "Language") "load" [| Jv.of_string wasm |]
    |> Fut.of_promise ~ok:id
  ;;

  let version lang = Jv.get lang "version" |> Jv.to_int
  let field_count lang = Jv.get lang "fieldCount" |> Jv.to_int

  let field_id_for_name lang name =
    Jv.call lang "fieldIdForName" [| Jv.of_string name |] |> Jv.to_int
  ;;

  let field_name_for_id lang name =
    Jv.call lang "fieldNameForId" [| Jv.of_int name |] |> Jv.to_string
  ;;

  let id_for_node_type lang type' named =
    Jv.call lang "idForNodeType" [| Jv.of_string type'; Jv.of_bool named |]
    |> Jv.to_option Jv.to_string
  ;;

  let node_type_count lang = Jv.get lang "nodeTypeCount" |> Jv.to_int

  let node_type_for_id lang type_id =
    Jv.call lang "nodeTypeForId" [| Jv.of_int type_id |] |> Jv.to_option Jv.to_string
  ;;

  let node_type_is_named lang type_id =
    Jv.call lang "nodeTypeIsNamed" [| Jv.of_int type_id |] |> Jv.to_bool
  ;;

  let node_type_is_visible lang type_id =
    Jv.call lang "nodeTypeIsVisible" [| Jv.of_int type_id |] |> Jv.to_bool
  ;;

  let query lang source = Jv.call lang "query" [| Jv.of_string source |]
end

module Parser = struct
  type t = parser
  type input = start_index:int -> ?start_point:position -> ?end_index:int -> string option

  module Options = struct
    type t = parser_options

    let new' ~included_ranges =
      Jv.obj [| "includedRanges", Jv.of_list id included_ranges |]
    ;;
  end

  let new' () = Jv.new' (tree_sitter ()) [||]
  let set_language parser lang = Jv.call parser "setLanguage" [| lang |] |> ignore
  let get_language parser = Jv.call parser "getLanguage" [||]

  let all_parser parser input previous_tree options =
    let options = match options with None -> Jv.null | Some opts -> opts in
    Jv.call parser "parse" [| input; previous_tree; options |]
  ;;

  let parse parser ?options source_code =
    all_parser parser (Jv.of_string source_code) Jv.null options
  ;;

  let reparse parser ?options tree source_code =
    all_parser parser (Jv.of_string source_code) tree options
  ;;

  let parse_structure parser ?options f = all_parser parser (Jv.repr f) Jv.null options

  let reparse_structure parser ?options tree f =
    all_parser parser (Jv.repr f) tree options
  ;;

  let reset parser = Jv.call parser "reset" [||] |> ignore

  let set_timeout_micros parser timeout =
    Jv.call parser "setTimeoutMicros" [| Jv.of_int timeout |] |> ignore
  ;;

  let get_timeout_micros parser = Jv.call parser "getTimeoutMicros" [||] |> Jv.to_int
  (* let set_logger parser cb = Jv.call parser "setLogger" [| Jv.repr cb |] *)
  (* let get_logger parser = Jv.call parser "getLogger" [| |] *)
end

module Tree = struct
  type t = tree

  let copy tree = Jv.call tree "copy" [||]
  let delete tree = Jv.call tree "delete" [||] |> ignore
  let edit tree edit = Jv.call tree "edit" [| edit |] |> ignore
  let root_node tree = Jv.get tree "rootNode"
  let get_language tree = Jv.call tree "getLanguage" [||]
  let walk tree = Jv.call tree "walk" [||]
  let get_changed_ranges t1 t2 = Jv.call t1 "getChangedRanges" [| t2 |]
end

module Node = struct
  type t = node

  let type_id node = Jv.get node "typeId" |> Jv.to_int
  let type' node = Jv.get node "type" |> Jv.to_string
  let end_position node = Jv.get node "endPosition"
  let end_index node = Jv.get node "endIndex" |> Jv.to_int
  let text node = Jv.get node "text" |> Jv.to_string
  let is_named node = Jv.call node "isNamed" [||] |> Jv.to_bool
  let has_error node = Jv.call node "hasError" [||] |> Jv.to_bool
  let has_changes node = Jv.call node "hasChanges" [||] |> Jv.to_bool
  let is_missing node = Jv.call node "isMissing" [||] |> Jv.to_bool
  let ( = ) a b = Jv.call a "equals" [| b |] |> Jv.to_bool
  let child node ix = Jv.call node "child" [| Jv.of_int ix |] |> to_option

  (* let named_child node ix = Jv.call node "namedChild" [| Jv.of_string ix |] *)
  (* let child_for_field_id node field_id = Jv.call node "childForFieldId" [| field_id |] *)

  (*
  let child_for_field_name node field_name =
    Jv.call node "childForFieldName" [| field_name |]
  ;;
     *)

  let child_count node = Jv.get node "childCount" |> Jv.to_int
  let named_child_count node = Jv.get node "namedChildCount" |> Jv.to_int
  let first_child node = Jv.get node "firstChild" |> to_option
  let first_named_child node = Jv.get node "firstNamedChild" |> to_option
  let last_child node = Jv.get node "lastChild" |> to_option
  let last_named_child node = Jv.get node "lastNamedChild" |> to_option
  let children node = Jv.get node "children" |> to_list
  let named_children node = Jv.get node "namedChildren" |> to_list

  (* TODO: descendantsOfType *)
  let next_sibling node = Jv.get node "nextSibling" |> to_option
  let previous_sibling node = Jv.get node "previousSibling" |> to_option
  let next_named_sibling node = Jv.get node "nextNamedSibling" |> to_option
  let previous_named_sibling node = Jv.get node "previousNamedSibling" |> to_option
  let parent node = Jv.get node "parent" |> to_option

  (* TODO: descendantForIndex, namedDescendantForIndex
   * descendantForPosition, namedDescendantForPosition
   * *)
  let walk node = Jv.call node "walk" [||]
  let to_string node = Jv.call node "toString" [||] |> Jv.to_string
end

module Tree_cursor = struct
  type t = tree_cursor

  let delete cursor = Jv.call cursor "delete" [||] |> ignore
  let reset cursor node = Jv.call cursor "reset" [| node |] |> ignore
  let node_type cursor = Jv.get cursor "nodeType" |> Jv.to_string
  let node_type_id cursor = Jv.get cursor "nodeTypeId" |> Jv.to_int
  let node_id cursor = Jv.get cursor "nodeId" |> Jv.to_int
  let node_is_named cursor = Jv.get cursor "nodeIsNamed" |> Jv.to_bool
  let node_is_missing cursor = Jv.get cursor "nodeIsMissing" |> Jv.to_bool
  let node_text cursor = Jv.get cursor "nodeText" |> Jv.to_string
  let start_position cursor = Jv.get cursor "startPosition"
  let end_position cursor = Jv.get cursor "endPosition"
  let start_index cursor = Jv.get cursor "startIndex" |> Jv.to_int
  let end_index cursor = Jv.get cursor "endIndex" |> Jv.to_int
  let current_node cursor = Jv.call cursor "currentNode" [||]
  let current_field_id cursor = Jv.call cursor "currentFieldId" [||] |> Jv.to_int

  let current_field_name cursor =
    Jv.call cursor "currentFieldName" [||] |> Jv.to_option Jv.to_string
  ;;

  let goto_first_child cursor = Jv.call cursor "gotoFirstChild" [||] |> Jv.to_bool
  let goto_next_sibling cursor = Jv.call cursor "gotoNextSibling" [||] |> Jv.to_bool
  let goto_parent cursor = Jv.call cursor "gotoParent" [||] |> Jv.to_bool
end

module Query = struct
  type t = query

  module Capture = struct
    type t =
      { name : string
      ; text : string
      }

    let of_jv jv =
      { name = Jv.get jv "name" |> Jv.to_string; text = Jv.get jv "text" |> Jv.to_string }
    ;;
  end

  module Match = struct
    type t =
      { pattern : int
      ; captures : Capture.t list
      }

    let of_jv jv =
      { pattern = Jv.get jv "pattern" |> Jv.to_int
      ; captures = Jv.get jv "captures" |> Jv.to_list Capture.of_jv
      }
    ;;
  end

  module Operand = struct
    type t =
      { type' : string
      ; name : string
      }

    let of_jv jv =
      { type' = Jv.get jv "type'" |> Jv.to_string
      ; name = Jv.get jv "name" |> Jv.to_string
      }
    ;;
  end

  module Predicate = struct
    type t =
      { operator : string
      ; operands : Operand.t list
      }

    let of_jv jv =
      { operator = Jv.get jv "operator" |> Jv.to_string
      ; operands = Jv.get jv "operands" |> Jv.to_list Operand.of_jv
      }
    ;;
  end

  let delete query = Jv.call query "delete" [||] |> ignore

  let matches query node start_pos end_pos =
    Jv.call query "matches" [| node; start_pos; end_pos |] |> Jv.to_list Match.of_jv
  ;;

  let captures query node start_pos end_pos =
    Jv.call query "captures" [| node; start_pos; end_pos |] |> Jv.to_list Capture.of_jv
  ;;

  let predicates_for_pattern query n =
    Jv.call query "predicatesForPattern" [| Jv.of_int n |] |> Jv.to_list Predicate.of_jv
  ;;

  let did_exceed_match_limit query =
    Jv.call query "didExceedMatchLimit" [||] |> Jv.to_bool
  ;;
end
