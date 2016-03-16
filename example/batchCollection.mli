(* BatchCollection - Persistant batch job journal

Lemonade Sqlite (https://github.com/michipili/lemonade-sqlite)
This file is part of Lemonade Sqlite

Copyright © 2016 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** The collection of available batch jobs. *)

val init : string -> Lemonade_Sqlite.handle
(** Intialise the collection, backed by the given file. *)

val with_batch_collection : string -> (Lemonade_Sqlite.handle -> 'a) -> 'a
(** Intialise the collection and run some treatment on it, then closes
    the collection. *)

val drop : string -> unit
(** Drop the collection, backed by the given file. *)

val insert : Batch.t Lemonade_Sqlite.S.t -> Lemonade_Sqlite.handle -> unit Lemonade_Sqlite.t
(** [insert s] insert each query batch enumerated by [s] into the
    collection. *)

val contents : Lemonade_Sqlite.handle -> Batch.t Lemonade_Sqlite.S.t
(** [contents ()] return a stream enumerating the batches found
    in the collection. *)

type performance = {
  tablename: string;
  duration: float;
  detail: (string * int * float) list;
}

val performance :  Lemonade_Sqlite.handle -> performance Lemonade_Sqlite.S.t
(** [performance()] return performance numbers for the batches found
    in the collection. *)
