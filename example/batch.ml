(* Batch - Batch job journal

Lemonade Sqlite (https://github.com/michipili/lemonade-sqlite)
This file is part of Lemonade Sqlite

Copyright © 2016 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Format

(* Compatibility with older versions of OCaml *)
let ( |> ) x f =
  f x

type t = batch
and batch = {
  batch_id: int;
  batch_received_at: string;
  batch_completed_at: string;
  batch_status: status;
  batch_query: query list;
  batch_response: response list;
}
and query = {
  query_timestamp: string;
  query_tablename: string;
  query_key: string;
}
and response = {
  response_timestamp: string;
  response_tablename: string;
  response_key: string;
  response_item_n: int;
  response_duration: float;
}
and status =
  | Success
  | Error

let make_query (query_timestamp, query_tablename, query_key) = {
  query_timestamp;
  query_tablename;
  query_key;
}

let make_response (
    response_timestamp,
    response_tablename,
    response_key,
    response_item_n,
    response_duration
  ) = {
  response_timestamp;
  response_tablename;
  response_key;
  response_item_n;
  response_duration
}

let make (
    batch_id,
    batch_received_at,
    batch_completed_at,
    spec_status,
    spec_query,
    spec_response
  ) = {
  batch_id;
  batch_received_at;
  batch_completed_at;
  batch_status = if spec_status then Success else Error;
  batch_query = List.map make_query spec_query ;
  batch_response = List.map make_response spec_response;
}

let format_list_generic delim_open delim_close ppt format_item lst =
  let rec loop ppt = function
    | hd :: [] -> format_item ppt hd
    | hd :: tl -> fprintf ppt "%a;@ " format_item hd; loop ppt tl
    | [] -> ()
  in
  fprintf ppt "@[<hv 2>%c %a %c@]" delim_open loop lst delim_close

let format_list ppt format_item lst =
  format_list_generic '[' ']' ppt format_item lst

type 'a format_record_spec =
  | Format_int of ('a -> int)
  | Format_string of ('a -> string)
  | Format_float of ('a -> float)

let format_record spec ppt record =
  let format_item ppt (label, formatter) =
    match formatter with
    | Format_int(get_int) -> fprintf ppt "@[ %s = %d; @]"
                               label (get_int record)
    | Format_string(get_string) -> fprintf ppt "@[ %s = %S; @]"
                                     label (get_string record)
    | Format_float(get_float) -> fprintf ppt "@[ %s = %5.3f; @]"
                                   label (get_float record)
  in
  format_list_generic '{' '}' ppt format_item spec

let pp_print_ocamlstring ppt s =
  fprintf ppt "%S" s

let format_query ppt lst =
  let format_item ppt query =
    format_record [
      "query_timestamp",
      Format_string(fun x -> x.query_timestamp);

      "query_tablename",
      Format_string (fun x -> x.query_tablename);

      "query_key",
      Format_string (fun x -> x.query_key);
    ] ppt query
  in
  format_list ppt format_item lst

let format_response ppt lst =
  let format_item ppt response =
    format_record [
      "response_timestamp",
      Format_string(fun x -> x.response_timestamp);

      "response_tablename",
      Format_string(fun x -> x.response_tablename);

      "response_key",
      Format_string(fun x -> x.response_key);

      "response_item_n",
      Format_int(fun x -> x.response_item_n);

      "response_duration",
      Format_float(fun x -> x.response_duration);
    ] ppt response
  in
  format_list ppt format_item lst

let format ppt batch =
  Format.fprintf ppt "@[<hv 2>{ @[batch_id = %d;@]@ @[batch_received_at = %S;@]@ @[batch_completed_at = %S;@]@ @[batch_status = %s;@]@ @[batch_query = %a;@]@ @[batch_response = %a;@] }@]"
    batch.batch_id
    batch.batch_received_at
    batch.batch_completed_at
    (match batch.batch_status with
     | Success -> "Success"
     | Error -> "Error")
    format_query batch.batch_query
    format_response batch.batch_response

let print batch =
  format Format.std_formatter batch

let random_tablename () =
  let tabledb = [|
    "fruit";
    "cake";
    "meat";
    "hardware";
    "grocery";
  |]
  in
  Array.get tabledb (Random.int (Array.length tabledb))

let random_key () =
  string_of_int (Random.int 50_000)

let random_item_n () =
  Random.int 1_000

let random id =
  let time_0 = Timestamp.to_time "2015-01-01T00:00:00.000Z" in
  let time_1 = Timestamp.to_time "2016-01-01T00:00:00.000Z" in
  let timestamp s =
    Timestamp.of_time (time_0 +. (time_1 -. time_0) *. s)
  in
  let duration_unit = 3.16881e-9  (* A tenth of a second, in years *) in
  let random_query_response arrival duration =
    let tablename = random_tablename () in
    let key = random_key () in
    ((timestamp arrival, tablename, key),
     (timestamp (arrival +. duration),
      tablename,
      key,
      random_item_n (),
      duration /. duration_unit /. 10.0))
  in
  let length =
    Array.to_list(Array.init (2 + (Random.int 8)) (fun _ -> ()))
  in
  let arrival = Random.float 1.0 in
  let random_duration () =
    Random.float duration_unit
  in
  let plan =
    List.map random_duration length
    |> List.fold_left
      (fun (ax, s) delta -> ((s, delta)::ax, s+.1.1*.delta))
      ([], arrival +. (random_duration()))
  in
  let qr = List.map
      (fun (arrival, duration) -> random_query_response arrival duration)
      (fst plan)
  in make (
    id,
    timestamp arrival,
    timestamp (snd plan),
    Random.bool (),
    List.map fst qr,
    List.map snd qr
  )
