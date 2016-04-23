(* Test_Lemonade_Sqlite -- Simple tests

Lemonade Sqlite (https://github.com/michipili/lemonade-sqlite)
This file is part of Lemonade Sqlite

Copyright © 2016 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

module Sqlite =
  Lemonade_Sqlite

open Printf
open Broken
open Sqlite.Infix

(* Compatibility with older versions of OCaml *)
let ( |> ) x f =
  f x

let log row =
  Format.fprintf Format.str_formatter "DEBUG: Log row: %a"
    Sqlite.pp_print_row row;
  eprintf "%s\n%!" (Format.flush_str_formatter ())

let log_handle handle =
  Format.fprintf Format.str_formatter "DEBUG: Log handle: %a"
    Sqlite.pp_print_handle handle;
  eprintf "%s\n%!" (Format.flush_str_formatter ())

let database () =
  "unit_testing.db"

let sqlrun f =
  Sqlite.run(Sqlite.withdb (database()) f)

let assert_monad name f x expected =
  let g x =
    match Sqlite.run (f x) with
    | Sqlite.Success(y) -> y
    | Sqlite.Error(err, mesg) ->
        ksprintf failwith "Assert_Monad.%s: %s" err mesg
  in
  assert_equal name g x expected

let sqlite3 sql =
  let lines c =
    let q = Queue.create () in
    begin
      try while true do
          Queue.push (input_line c) q
        done
      with End_of_file -> ()
    end;
    Queue.fold (fun ax s -> s :: ax) [] q
    |> List.rev
  in
  let cmd =
    sprintf "sqlite3 %s %S" (database()) sql
  in
  let c = Unix.open_process_in cmd in
  let answer = lines c in
  ignore(Unix.close_process_in c);
  answer

let assert_sqlite_exec name m sql expected =
  let init () =
    match sqlrun m with
    | Sqlite.Success() -> ()
    | Sqlite.Error(name, mesg) -> ksprintf failwith "%s: %s" name mesg
  in
  assert_equal name (fun sql -> init(); sqlite3 sql) sql expected

let assert_sqlite_query name sql answer =
  let rows_to_list stream =
    Sqlite.S.to_list
      (Sqlite.S.filter_map
         (fun row ->
            match row.(0) with
            | Sqlite.TEXT(text) -> Some(text)
            | _ -> None) stream)
  in
  let result () =
    match sqlrun begin fun handle ->
        Sqlite.query (Sqlite.statement sql) handle
        |> rows_to_list
      end
    with
    | Sqlite.Success(x) -> x
    | Sqlite.Error(name, mesg) -> ksprintf failwith "%s: %s" name mesg
  in
  assert_equal name result () answer

let test_open_create () =
  assert_true "open_create" begin fun () ->
    match sqlrun (fun _ -> Sqlite.return ()) with
    | Sqlite.Success() -> Sys.file_exists (database())
    | Sqlite.Error(name, mesg) -> ksprintf failwith "%s: %s" name mesg
  end ()

let test_create_table () =
  let sql =
    "DROP TABLE IF EXISTS a;\
     CREATE TABLE a (number INT, name TEXT)"
  in
  assert_sqlite_exec "create_table"
    (Sqlite.exec (Sqlite.statement sql))
    ".tables"
    [ "a" ]

let test_insert () =
  let sql = "INSERT INTO a VALUES($number, $name)" in
  let bindings = Sqlite.bindings [
      "$number", (fun (number, _) -> Sqlite.INT(Int64.of_int number));
      "$name", (fun (_, name) -> Sqlite.TEXT(name));
    ]
  in
  let data = Sqlite.S.of_list [
      1, "apple";
      2, "pear";
    ]
  in
  assert_sqlite_exec "insert"
    (Sqlite.insert
       (Sqlite.bindings_apply
          (bindings data)
          (Sqlite.statement sql)))
    "SELECT * FROM a ORDER BY number ASC"
    [
      "1|apple";
      "2|pear";
    ]

let test_rowid () =
  let sql = "INSERT INTO a VALUES($number, $name)" in
  let data = Sqlite.S.of_list [
      3, "peach";
      4, "lemon";
    ]
  in
  let bindings = Sqlite.bindings [
      "$number", (fun (number, _) -> Sqlite.INT(Int64.of_int number));
      "$name", (fun (_, name) -> Sqlite.TEXT(name));
    ]
  in
  let prog handle =
      Sqlite.insert
        (Sqlite.bindings_apply
           (bindings data)
           (Sqlite.statement sql))
        handle
  in
  assert_sqlite_exec "rowid"
    prog
    "SELECT * FROM a ORDER BY number ASC"
    [
      "1|apple";
      "2|pear";
      "3|peach";
      "4|lemon";
    ]

let test_query () =
  assert_sqlite_query "query"
      "SELECT name FROM a ORDER BY NAME ASC"
      [ "apple"; "lemon"; "peach"; "pear" ]

let test_project () =
  let makerow data = Array.map (fun s -> Sqlite.TEXT(s)) data in
  let challenge = List.map makerow [
      [| "a"; "x"; "1"; |];
      [| "a"; "y"; "2"; |];
      [| "b"; "z"; "3"; |];
    ]
  in
  let answer = [
    ("a", [ ("x", 1); ("y", 2)]);
    ("b", [ ("z", 3) ])
  ]
  in
  let p row =
    let open Sqlite in
    match row with
    | [| TEXT(base); TEXT(sigma1); TEXT(sigma2); |] ->
      (base, (sigma1, int_of_string sigma2))
    | _ -> invalid_arg "protocol error"
  in
  let f x =
    match sqlrun begin fun _ ->
        Sqlite.S.to_list
          ((Sqlite.project p (Sqlite.S.of_list x)))
      end with
    | Sqlite.Success(x) -> x
    | Sqlite.Error(name, mesg) -> ksprintf failwith "%s: %s" name mesg
  in
  assert_equal "project" f challenge answer


let test_insert_concat () =
  let init = "DROP TABLE IF EXISTS b;\
     CREATE TABLE b (number INT, name TEXT);\
     DROP TABLE IF EXISTS c;\
     CREATE TABLE c (number INT, name TEXT)"
  in
  let insert_b =
    Sqlite.statement "INSERT INTO b VALUES($number, $name)"
  in
  let insert_c =
    Sqlite.statement "INSERT INTO c VALUES($number, $name)"
  in
  let bindings lst = Sqlite.bindings [
      "$number", (fun (number, _) -> Sqlite.INT(Int64.of_int number));
      "$name", (fun (_, name) -> Sqlite.TEXT(name));
    ] (Sqlite.S.of_list lst)
  in
  let is_odd (k, _) =
    k mod 2 = 1
  in
  let is_even x =
    not(is_odd x)
  in
  let data = [
      1, "apple";
      2, "pear";
      3, "pineapple";
      4, "strawberry";
    ]
  in
  assert_sqlite_exec "insert_concat"
    begin fun db ->
      Sqlite.insert begin
        Sqlite.S.concat (Sqlite.S.of_list [
            Sqlite.S.of_list [
              Sqlite.statement init
            ];
            Sqlite.bindings_apply
              (bindings (List.filter is_odd data))
              insert_b;
            Sqlite.bindings_apply
              (bindings (List.filter is_even data))
              insert_c;
          ])
      end db
    end
    "SELECT * FROM b ORDER BY number ASC;SELECT * FROM c ORDER BY number ASC"
    [
      "1|apple";
      "3|pineapple";
      "2|pear";
      "4|strawberry";
    ]


let () = register_suite "Sqlite"
    "Test the monadic sqlite interface" [

    assert_monad "stream_of_list"
      (fun lst -> Sqlite.S.to_list (Sqlite.S.of_list lst))
      [ "a"; "b" ]
      [ "a"; "b" ];

    test_open_create ();
    test_create_table ();
    test_insert ();
    test_rowid ();
    test_query ();
    test_project ();
    test_insert_concat ();
  ]
