open Brr
open Brr_web_tree_sitter
open Web_tree_sitter

let test lbl success =
  let msg, color = if success then lbl ^ ": okay", "green" else lbl ^ ": FAILED", "red" in
  let elem = El.(p [ txt' msg ]) in
  El.(set_inline_style Style.color (Jstr.v color) elem);
  El.append_children (Document.body G.document) [ elem ]
;;

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
  test "Language.version" (Language.version lang = 13);
  test "Language.field_count" (Language.field_count lang = 0);
  test "Language.node_type_count" (Language.node_type_count lang = 10)
;;

let (_ : unit Fut.t) =
  let open Fut.Syntax in
  let* _ = unnamed_lang in
  let+ parser = parser in
  let tree = Parser.parse parser "x * x" in
  let x = tree |> Tree.root_node |> Node.to_string in
  test "tree |> Tree.root_node |> Node.to_string" (x = "(expr (expr) (expr))");
  Console.log [ x ];
  (* TODO: broken *)
  let tree = Parser.reparse parser tree "x * x + y" in
  let x = tree |> Tree.root_node |> Node.to_string in
  test
    "tree |> Tree.root_node |> Node.to_string"
    (x = "(expr (expr (expr) (expr)) (expr))");
  Console.log [ x ];
  let tree = Parser.parse parser "x * x + y" in
  let x = tree |> Tree.root_node |> Node.to_string in
  test
    "tree |> Tree.root_node |> Node.to_string"
    (x = "(expr (expr (expr) (expr)) (expr))");
  Console.log [ x ];
  let node = Tree.root_node tree in
  test "Node.text" (Node.text node = "x * x + y");
  test "Node.is_named" (Node.is_named node = true);
  (match Node.first_child node with
  | None -> test "Node.first_child" false
  | Some node' ->
    test "Node.(=)" (not Node.(node' = node));
    test "Node.text" (Node.text node' = "x * x"));
  let cursor = Tree.walk tree in
  test "Tree_cursor.node_type" (Tree_cursor.node_type cursor = "expr");
  test "Tree_cursor.node_text" (Tree_cursor.node_text cursor = "x * x + y");
  let start_pos = Tree_cursor.start_position cursor in
  test "Tree_cursor.start_position" Position.(row start_pos = 0 && column start_pos = 0);
  let end_pos = Tree_cursor.end_position cursor in
  test "Tree_cursor.end_position" Position.(row end_pos = 0 && column end_pos = 9);
  test "Tree_cursor.start_index" (Tree_cursor.start_index cursor = 0);
  test "Tree_cursor.end_index" (Tree_cursor.end_index cursor = 9);
  Console.log
    Tree_cursor.
      [ node_type_id cursor
      ; node_id cursor
      ; node_is_named cursor
      ; node_is_missing cursor
      ; current_field_id cursor
        (* TODO: broken ; current_field_name cursor *)
      ]
;;
