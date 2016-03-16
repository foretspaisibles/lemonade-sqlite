(* BatchCollection - Persistant batch job journal

Lemonade Sqlite (https://github.com/michipili/lemonade-sqlite)
This file is part of Lemonade Sqlite

Copyright © 2016 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

module Sqlite = Lemonade_Sqlite
open Sqlite.Infix

(* Compatibility with older versions of OCaml *)
let ( |> ) x f =
  f x

let not_implemented identifier =
  Pervasives.failwith(Printf.sprintf "BatchCollection.%s: Not implemented." identifier)

let failwith fmt =
  let prefix = "BatchCollection." in
  Printf.ksprintf (fun s -> Pervasives.failwith (prefix ^ s)) fmt

type performance = {
  tablename: string;
  duration: float;
  detail: (string * int * float) list;
}

let init file =
  Sqlite.opendb ~init:SqlPool.init file

let with_batch_collection file m =
  Sqlite.withdb ~init:SqlPool.init file m

let drop file =
  if Sys.file_exists file then
    Sys.remove file
  else
    ()

let insert stream handle =
  let current_batch_id = ref Int64.zero in
  let current_batch_query = ref Int64.zero in
  let current_batch_response = ref Int64.zero in

  let insert_batch_index =
    Sqlite.statement SqlPool.insert_batch_index
  in
  let insert_batch_query =
    Sqlite.statement SqlPool.insert_batch_query
  in
  let insert_batch_response =
    Sqlite.statement SqlPool.insert_batch_response
  in
  let bindings_index batch =
    let open Batch in
    Sqlite.bindings [
      "$received_at", (fun batch -> Sqlite.TEXT(batch.batch_received_at));
      "$completed_at", (fun batch -> Sqlite.TEXT(batch.batch_completed_at));
      "$status",
      (fun batch -> Sqlite.INT(match batch.batch_status with
           | Success -> Int64.zero
           | Error -> Int64.one));
    ] (Sqlite.S.of_list [ batch ])
  in
  let bindings_query lst =
    let open Batch in
    Sqlite.bindings [
      "$batch_id",
      (fun _ -> Sqlite.INT(!current_batch_id));

      "$timestamp",
      (fun query -> Sqlite.TEXT(query.query_timestamp));

      "$tablename",
      (fun query -> Sqlite.TEXT(query.query_tablename));

      "$key",
      (fun query -> Sqlite.TEXT(query.query_key));
    ] (Sqlite.S.of_list lst)
  in
  let bindings_response lst =
    let open Batch in
    Sqlite.bindings [
      "$batch_id",
      (fun _ -> Sqlite.INT(!current_batch_id));

      "$timestamp",
      (fun response -> Sqlite.TEXT(response.response_timestamp));

      "$tablename",
      (fun response -> Sqlite.TEXT(response.response_tablename));

      "$key",
      (fun response -> Sqlite.TEXT(response.response_key));

      "$item_n",
      (fun response -> Sqlite.INT(Int64.of_int response.response_item_n));

      "$duration",
      (fun response -> Sqlite.FLOAT(response.response_duration));

    ] (Sqlite.S.of_list lst)
  in

  let really_insert_index batch =
    Sqlite.bindings_apply ~rowid:current_batch_id
      (bindings_index batch)
      insert_batch_index
  in
  let really_insert_query batch =
    Sqlite.bindings_apply ~rowid:current_batch_query
      (bindings_query batch.Batch.batch_query)
      insert_batch_query
  in
  let really_insert_response batch =
    Sqlite.bindings_apply ~rowid:current_batch_response
      (bindings_response batch.Batch.batch_response)
      insert_batch_response
  in
  let really_insert batch =
    let ( @ ) = Sqlite.S.append in
    really_insert_index batch
    @ really_insert_query batch
    @ really_insert_response batch
  in
  Sqlite.insert
    (Sqlite.S.(concat (map really_insert stream))) handle

let contents handle =
  let p row =
    let open Sqlite in
    match row with
    | [|
      INT(batch_id);
      TEXT(batch_received_at);
      TEXT(batch_completed_at);
      INT(batch_status);
      _;
      TEXT(tablename);
      TEXT(key);
      TEXT(query_timestamp);
      TEXT(response_timestamp);
      INT(item_n);
      FLOAT(duration);
    |] -> ({
        Batch.
        batch_id = Int64.to_int batch_id;
        batch_received_at;
        batch_completed_at;
        batch_status =
          (match Int64.to_int batch_status with
           | 0 -> Batch.Success
           | 1 -> Batch.Error
           | _ -> failwith "protocol error");
        batch_query = [];
        batch_response = [];
      }, ({
        Batch.
        query_timestamp;
        query_tablename = tablename;
        query_key = key;
      }, Some({
        Batch.
        response_timestamp;
        response_tablename = tablename;
        response_key = key;
        response_item_n = Int64.to_int item_n;
        response_duration = duration;
      })))
    | whatever -> failwith "protocol error"
  in
  let rec response ax lst =
    match lst with
    | Some(r) :: tl -> response (r :: ax) tl
    | None :: tl -> response ax tl
    | [] -> List.rev ax
  in
  let finalize (base, fibre) =
    match base.Batch.batch_status with
    | Batch.Success -> {
        base with
        Batch.
        batch_query = List.map fst fibre;
        batch_response = response [] (List.map snd fibre);
      }
    | Batch.Error -> { base with Batch.batch_query = List.map fst fibre; }
  in
  Sqlite.query (Sqlite.statement SqlPool.query_contents) handle
  |> Sqlite.project p
  |> Sqlite.S.map finalize

let performance _ =
  not_implemented "performance"
