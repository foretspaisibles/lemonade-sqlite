(* Lemonade_sqlite -- Monadic interface for sqlite

Lemonade Sqlite (https://github.com/michipili/lemonade-sqlite)
This file is part of Lemonade Sqlite

Copyright © 2016 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Monadic interface to SQlite3.

This monad is a composition of a [Success] monad holding computations
that can fail and of a [Reader] monad for computations reading a
database handle and some caching information. *)


(** {6 The Sqlite Monad} *)
include Lemonade_Type.S

(** {6 Operations in the success monad} *)

type error = string * string
(** The type of sqlite errors. *)

(** The outcome of computations throwing errors. *)
type (+'a) outcome =
  | Success of 'a
  | Error of error


val run : 'a t -> 'a outcome
(** Reveal the outcome of a computation with errors. *)

val error : error -> 'a t
(** Fail with the given error. *)

val recover : 'a t -> (error -> 'a t) -> 'a t
(** [recover m handler] is a monad containing the same value as [m]
    and thrown errors are interepreted by the [handler]. *)

type handle
(** The type of database handle. *)

(** {6 The Stream Monad} *)
module S : Lemonade_Stream.S
  with type 'a monad = 'a t

val opendb : ?init:string -> string -> handle
(** Open a handle to a database.

    If [init] is provided, this sql statement is used to initialise
    the database, in case the database does not already exist. *)

val closedb : handle -> unit
(** Close a handle to a database. *)

val withdb : ?init:string -> string -> (handle -> 'a) -> 'a
(** [withdb f] safely applies [f] on a handle opened on
    the given database.

    If [init] is provided, this sql statement is used to initialise
    the database, in case the database does not already exist. *)


(** {6 Database related types} *)

type row = data array
(** The type of database rows returned by queries. *)

(** The type of data held by our database. *)
and data = Sqlite3.Data.t =
  | NONE
  | NULL
  | INT of int64
  | FLOAT of float
  | TEXT of string
  | BLOB of string

type statement
(** The abstract type of SQL statements. *)

type binding = (string * (unit -> data)) list
(** The type of variable binding definitions. These can be applied
    to statements or used with a query. *)

val query : ?binding:binding -> statement -> handle -> row S.t
(** [query statement] is the stream of rows returned by the
    [statement]. *)

val one : row S.t -> row t
(** [one rows] convert a stream of rows into one row. It will
    return an [Error] if the input stream has not exactly one row. *)

val maybe : row S.t -> row option t
(** [maybe rows] convert a stream of rows into one or zero rows.
    It will return an [Error] if the input stream has more than one row. *)

val project : ('a -> 'b * 'c ) -> 'a S.t -> ('b * 'c list) S.t
(** [project p stream] construct a stream by applying the projector
    [p] on stream items, to extract a base and a fibre value.  Consecutive
    fibre values above a given [base] are bundled together. *)

val exec : statement -> handle -> unit t
(** [exec commmands] is a monad applying the statements provided by
    [commands] and ignoring produced rows if any. *)

val insert : statement S.t -> handle -> unit t
(** [insert commmands] is a monad applying the statements provided by
    [commands] and ignoring produced rows if any. *)


(** {6 Statements} *)

val statement : string -> statement
(** [statement sql] return the statement corresponding to [sql].
    Complex statements are supported. *)

val rowid_binding : int64 ref -> ('a -> data)
(** Make a function returning the given rowid in the form of sqlite data. *)


(** {6 Bindings} *)

val bindings: (string * ('a -> data)) list -> 'a S.t -> binding S.t
(** [bindings spec data] turn a binding specification and data stream into
    a binding stream. The specification is made of pairs [(name, get)] which
    identify which variable to bind and which data to bind it to. *)

val bindings_apply : ?rowid:int64 ref -> binding S.t -> statement -> statement S.t
(** [bindings_apply bindings stmt] create a statement stream by
    repetitively using the bindings provided by [bindings] on [stmt].

    If the argument [rowid] is given, the statements generated by the
    binding arrange so that the [rowid] is updated with the
    [last_insert_rowid] value after execution of the statement. *)

(** {6 Pretty printing} *)

val pp_print_data : Format.formatter -> data -> unit
(** Pretty printer for sqlite data. *)

val pp_print_row : Format.formatter -> row -> unit
(** Pretty printer for sqlite data rows. *)
