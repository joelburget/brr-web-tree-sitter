open Brr
open Brr_web_tree_sitter
open Web_tree_sitter

let () = El.set_children (Document.body G.document) El.[ txt' "(look in the console)" ]
let unnamed_lang, fulfil_unnamed_lang = Fut.create ()
let parser, fulfil_parser = Fut.create ()

let (_ : unit Fut.or_error) =
  let open Fut.Result_syntax in
  let+ () = Web_tree_sitter.init () in
  let parser = Parser.new' () in
  fulfil_parser parser
;;

let (_ : unit Fut.or_error) =
  let open Fut.Result_syntax in
  let* parser = Fut.map (fun p -> Ok p) parser in
  let+ lang = Language.load "tree-sitter-unnamed.wasm" in
  Parser.set_language parser lang;
  fulfil_unnamed_lang lang
;;

let (_ : unit Fut.t) =
  let open Fut.Syntax in
  let+ lang = unnamed_lang in
  Console.log Language.[ version lang; field_count lang; node_type_count lang ]
;;

let (_ : unit Fut.t) =
  let open Fut.Syntax in
  let* _ = unnamed_lang in
  let+ parser = parser in
  let tree = Parser.parse parser "x * x" in
  Console.log [ tree |> Tree.root_node |> Node.to_string ];
  (* TODO: broken *)
  let tree = Parser.reparse parser tree "x * x + y" in
  Console.log [ tree |> Tree.root_node |> Node.to_string ];
  let tree = Parser.parse parser "x * x + y" in
  Console.log [ tree |> Tree.root_node |> Node.to_string ];
  let node = Tree.root_node tree in
  Console.log Node.[ text node; is_named node ];
  (match Node.first_child node with
  | None -> Console.log [ "first_child -> None" ]
  | Some node' -> Console.log Node.[ node' = node; text node' ]);
  let cursor = Tree.walk tree in
  Console.log
    Tree_cursor.
      [ node_type cursor
      ; node_type_id cursor
      ; node_id cursor
      ; node_is_named cursor
      ; node_is_missing cursor
      ; node_text cursor
      ; start_position cursor
      ; end_position cursor
      ; start_index cursor
      ; end_index cursor
      ; current_field_id cursor
        (* TODO: broken ; current_field_name cursor *)
      ]
;;
