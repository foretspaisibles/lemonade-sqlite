(* Batch - Batch job journal

Lemonade Sqlite (https://github.com/michipili/lemonade-sqlite)
This file is part of Lemonade Sqlite

Copyright © 2016 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** A complex type for batch requests.

    This data type keeps track of hypothetical {i batch of queries}. Each batch
    consists of several queries and the corresponding responses.  The purpose
    of this data type is to fill a normalised database with random data. *)

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

val format : Format.formatter -> t -> unit
(** Formatter for query batches. *)

val print : t -> unit
(** Printer for query batches. *)

val random : int -> t
(** [random batch_id] create a random batch with the given [batch_id]. *)
