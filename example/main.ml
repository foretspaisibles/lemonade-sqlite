(* Main - Batch job journal

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
open Sqlite.Infix

(* Compatibility with older versions of OCaml *)
let ( |> ) x f =
  f x

let exit_success () =
  Sqlite.return 0

let exit_usage () =
  Sqlite.return 64

let exit_failure () =
  Sqlite.return 1

let database () =
  "./batch_collection.db"

let init _ =
  exit_success ()

let drop _ =
  exit_success begin
    BatchCollection.drop (database())
  end

let populate handle =
  let batch_list_sz = 50 in
  let batch_stream =
    Array.init batch_list_sz (fun i -> Batch.random i)
    |> Array.to_list
    |> Sqlite.S.of_list
  in
  BatchCollection.insert batch_stream handle
  >>= exit_success

let dump handle =
  Sqlite.S.iter Batch.print
    (BatchCollection.contents handle)
  >>= exit_success

let usage _ =
  Printf.eprintf "Usage: test_sqlite [-I|-D|-p|-d]";
  exit_usage()

let main handle =
  let action =
    try begin match Sys.argv.(1) with
    | "-I" -> init
    | "-D" -> drop
    | "-p" -> populate
    | "-d" -> dump
    | _ -> usage
    end with Invalid_argument("index out of bounds") -> usage
  in
  Sqlite.run (action handle)

let run t =
  match BatchCollection.with_batch_collection "batch.db" t with
  | Sqlite.Success k -> exit k
  | Sqlite.Error(name, mesg) -> eprintf "%s: %s\n" name mesg; exit 1

let () = run main
