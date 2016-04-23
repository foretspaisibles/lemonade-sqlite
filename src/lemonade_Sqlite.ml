open Printf

module Maybe =
  Lemonade_Maybe

let not_implemented identifier =
  ksprintf failwith "Lemonade_Sqlite.%s: Not implemented." identifier

(* Compatibility with older versions of OCaml *)
let ( |> ) x f =
  f x

type row = data array
and data = Sqlite3.Data.t =
  | NONE
  | NULL
  | INT of int64
  | FLOAT of float
  | TEXT of string
  | BLOB of string

let pp_print_blob pp s =
  let open Format in
  String.iter (fun c -> fprintf pp "\\x%2x" (Char.code c)) s

let pp_print_data ff data =
  let open Format in
  match data with
  | NONE -> fprintf ff "NONE"
  | NULL -> fprintf ff "NULL"
  | INT(n) -> fprintf ff "INT(%Ld)" n
  | FLOAT(x) -> fprintf ff "FLOAT(%f)" x
  | TEXT(s) -> fprintf ff "TEXT(%S)" s
  | BLOB(s) -> fprintf ff "BLOB(%a)" pp_print_blob s

let pp_print_row ff row =
  let open Format in
  let flag = ref false in
  let loop item =
    if !flag then fprintf ff ";@ ";
    flag := true;
    fprintf ff "%a" pp_print_data item
  in
  fprintf ff "@[<hov 2>[|";
  Array.iter loop row;
  fprintf ff "|]@]"

let debug label f x =
  Format.fprintf Format.str_formatter "DEBUG: %s: %a"
    label f x;
  eprintf "%s\n%!" (Format.flush_str_formatter ())

module Error =
struct
  type t = string * string
end

module Success =
struct
  include Lemonade_Success.Make(Error)

  let errorf name fmt =
    ksprintf (fun desc -> error(name, desc)) fmt

  let errorm name desc =
    error(name, desc)

  let unsafe_run m =
    match run m with
    | Success(x) -> x
    | Error(name, mesg) -> ksprintf failwith "Lemonade_sqlite.%s: %s"
                             name mesg

  (* Failing cleanly *)
  let failwith_anonymous_free_variable sql k =
    errorf
      "anonymous_free_variable"
      "Anonymous free variables are not supported. \
       The %d-th argument in %S is anonymous." k sql

  let failwith_free_variable name =
    errorf
      "free_variable"
      "The variable '%s' is not bound in this statement." name

  let failwith_range_error name k max =
    errorf name "range error: %d: %d" k max

  let failwith_sqlite3 name rc handle =
    errorf name "sqlite3: %s: %s" (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg handle)

  let failwith_callback_error name exn =
    errorf name "callback error: %s" (Printexc.to_string exn)

  let debug handle =
    eprintf "DEBUG Lemonade_sqlite %s: %s\n"
      (Sqlite3.Rc.to_string (Sqlite3.errcode handle))
      (Sqlite3.errmsg handle)

  let snoop name rc =
    eprintf "DEBUG Lemonade_sqlite %s: %s\n"
      name (Sqlite3.Rc.to_string rc);
    rc

  let debug_error handle name description =
    debug handle;
    errorm name description

  let log_error label m =
    recover m
      (function (name, mesg) as err -> eprintf "%s %s: %s\n%!" label name mesg; error err)

  let db_open ?mode ?mutex ?cache ?vfs filename =
    try return(Sqlite3.db_open ?mode ?mutex ?cache ?vfs filename)
    with Sqlite3.Error(message) -> errorm "db_open" message

  let _supervise handle name f x =
    try return(f x)
    with Sqlite3.Error(message) -> (debug handle; errorm name message)

  let db_close handle =
    try return(Sqlite3.db_close handle)
    with Sqlite3.Error(message) -> debug_error handle "db_close" message

  let prepare handle sql =
    try return(Sqlite3.prepare handle sql)
    with Sqlite3.Error(message) -> debug_error handle "prepare" message

  let prepare_tail statement =
    try return(Sqlite3.prepare_tail statement)
    with Sqlite3.Error(message) -> errorm "prepare_tail" message

  let step statement =
    try return(Sqlite3.step statement)
    with Sqlite3.Error(message) -> errorm "step" message

  let finalize statement =
    try return(Sqlite3.finalize statement)
    with Sqlite3.Error(message) -> errorm "finalize" message

  let finalize_safe statement =
    try return(Sqlite3.finalize statement)
    with Sqlite3.Error(message) -> return Sqlite3.Rc.OK

  let reset statement =
    try return(Sqlite3.reset statement)
    with Sqlite3.Error(message) -> errorm "reset" message

  let column_count statement =
    try return(Sqlite3.column_count statement)
    with Sqlite3.Error(message) -> errorm "column_count" message

  let column statement k =
    try return(Sqlite3.column statement k)
    with Sqlite3.Error(message) -> errorm "column" message

  let column_name statement k =
    try return(Sqlite3.column_name statement) with
    | Sqlite3.Error(message) -> errorm "column_name" message
    | Sqlite3.RangeError(k,max) -> failwith_range_error "column_name" k max

  let column_count statement =
    try return(Sqlite3.column_count statement)
    with Sqlite3.Error(message) -> errorm "column_count" message

  let column_count statement =
    try return(Sqlite3.column_count statement)
    with Sqlite3.Error(message) -> errorm "column_count" message

  let bind_parameter statement index data =
    try return(Sqlite3.bind statement index data) with
    | Sqlite3.Error(message) -> errorm "bind" message
    | Sqlite3.RangeError(k, max) -> failwith_range_error "bind" k max

  let bind_parameter_count statement =
    try return(Sqlite3.bind_parameter_count statement)
    with Sqlite3.Error(message) -> errorm "bind_parameter_count" message

  let bind_parameter_index statement k =
    try return(Sqlite3.bind_parameter_index statement k)
    with Sqlite3.Error(message) -> errorm "bind_parameter_index" message

  let bind_parameter_name statement k =
    try return(Sqlite3.bind_parameter_name statement k)
    with Sqlite3.Error(message) -> errorm "bind_parameter_name" message

  let last_insert_rowid handle =
    try return(Sqlite3.last_insert_rowid handle)
    with Sqlite3.Error(message) -> errorm "last_insert_rowid" message

  let stepthrough f lst =
    let rec loop lst rc =
      match rc with
      | Sqlite3.Rc.OK
      | Sqlite3.Rc.DONE -> step lst rc
      | whatever -> return whatever
    and step lst rc =
      match lst with
      | hd :: tl -> bind (f hd) (loop tl)
      | [] -> return rc
    in
    loop lst Sqlite3.Rc.OK

  let iter f lst =
    let rec loop lst () =
      match lst with
      | hd :: tl -> bind (f hd) (loop tl)
      | [] -> return ()
    in
    loop lst ()

  let rec fold f lst ax =
    match lst with
    | hd :: tl -> bind (f hd ax) (fold f lst)
    | [] -> return ax
end

module S =
  Lemonade_Stream.Make(Success)

module Binding =
struct
  type t = (string * data) list
  let empty = []

  let pp_print pp binding =
    let open Format in
    let pp_print_item pp (key, value) =
      fprintf pp "@[<hv 2>%S,@ %a@]" key pp_print_data value
    in
    Lemonade_List.pp_print pp_print_item pp binding
end

module CompiledStatement : sig
  type t and statement
  val make : string -> Sqlite3.db -> t Success.t
  val statements : t -> statement list Success.t
  val exec : Binding.t -> statement -> unit Success.t
  val query : Binding.t -> statement -> row S.t
  val finalize : t -> unit Success.t
  val pp_print : Format.formatter -> t -> unit
end = struct
  open Success.Infix

  module SuccessMaybe =
    Success.T(Maybe)

  type t = {
    sql: string;
    handle: Sqlite3.db;
    mutable code: statement list;
  }
  and statement = {
    statement: Sqlite3.stmt;
    variables: (string * int) list;
    variables_n : int;
    shandle: Sqlite3.db;
  }

  let pp_print_statement pp s =
    let open Format in
    let pp_print_pair pp (label, index) =
      fprintf pp "@[<hv 1>(%S,@ %d)@]" label index
    in
    let pp_print_variables pp lst =
      Lemonade_List.pp_print pp_print_pair pp lst
    in
    fprintf pp
      "@[<hv 1>CompiledStatement.{@ \
       @[<hv 1>statement =@ <abstr>@];@ \
       @[<hv 1>variables =@ %a@];@ \
       @[<hv 1>variables_n =@ %d@];@ \
                @[<hv 1>shandle =@ <abstr>@];@ \
       @]}"
      pp_print_variables s.variables
      s.variables_n

  let pp_print pp x =
    let open Format in
    fprintf pp
      "@[<hv 1>CompiledStatement.{@ \
       @[<hv 1>sql =@ %S@];@ \
       @[<hv 1>handle =@ <abstr> @[<hov 3>(* %s:@ %s *)@]@];@ \
       @[<hv 1>code =@ %a@];@ \
       @]}"
      x.sql
      (Sqlite3.Rc.to_string (Sqlite3.errcode x.handle))
      (Sqlite3.errmsg x.handle)
      (Lemonade_List.pp_print pp_print_statement) x.code

  let make_statement shandle sql stmt =
    let rec variables stmt ax k n =
      if k > n then
        Success.return (n, ax)
      else begin
        Success.bind_parameter_name stmt k
        >>= function
        | Some(name) -> variables stmt ((name,k)::ax) (k+1) n
        | None -> Success.failwith_anonymous_free_variable sql k
      end
    in
    Success.bind_parameter_count stmt
    >>= variables stmt [] 1
    >>= fun (variables_n, variables) ->
    Success.return {
      statement = stmt;
      variables;
      variables_n;
      shandle;
    }

  let precompile handle sql =
    let rec loop ax stmt =
      Success.prepare_tail stmt
      >>= begin function
        | None -> Success.return (return (List.rev ax))
        | Some(tail) ->
            make_statement handle sql tail >>= pack ax
      end
    and pack ax y =
      loop (y::ax) y.statement
    and return ax =
      ax
    in
    Success.prepare handle sql
    >>= make_statement handle sql
    >>= pack []

  let make sql handle =
    precompile handle sql
    >>= fun code ->
    Success.return {
      sql;
      handle;
      code;
    }

    let statements x =
      Success.return x.code

  let bind_statement binding stmt =
    let callback name =
      try Success.return(List.assoc name binding)
      with Not_found -> Success.failwith_free_variable name
    in
    let bind_parameter (name, k) =
      callback name
      >>= fun x ->
      Success.bind_parameter stmt.statement k x
      >>= function
      | Sqlite3.Rc.OK -> Success.return ()
      | Sqlite3.Rc.RANGE ->
          Success.failwith_range_error "CompiledStatement.bind_statement" k stmt.variables_n
      | whatever ->
          Success.failwith_sqlite3 "CompiledStatement.bind_statement" whatever stmt.shandle
    in
    let rec loop lst () =
      match lst with
      | [] -> Success.return ()
      | hd :: tl -> bind_parameter hd >>= loop tl
    in
    Success.reset stmt.statement
    >>= function
    | Sqlite3.Rc.OK -> loop stmt.variables ()
    | whatever -> Success.failwith_sqlite3 "CompiledStatement.bind_statement/reset" whatever stmt.shandle

  let exec binding stmt =
    let rec loop () =
      Success.step stmt.statement
      >>= function
      | Sqlite3.Rc.OK
      | Sqlite3.Rc.DONE -> Success.return ()
      | Sqlite3.Rc.ROW -> loop ()
      | whatever -> Success.failwith_sqlite3 "CompiledStatement.exec" whatever stmt.shandle
    in
    bind_statement binding stmt
    >>= loop

  let retrieve_rows stmt m =
    let exhausted = ref false in
    let rec f _ =
      if !exhausted then
        Success.return None
      else
        m
        >>= fun () -> Success.step stmt.statement
        >>= function
        | Sqlite3.Rc.OK
        | Sqlite3.Rc.DONE -> (exhausted := true; Success.return None)
        | Sqlite3.Rc.ROW -> Success.return(Some(Sqlite3.row_data stmt.statement))
        | whatever -> Success.failwith_sqlite3 "CompiledStatement.stepthrough_rows" whatever stmt.shandle
    in
    f

  let query binding stmt =
    S.from(retrieve_rows stmt (bind_statement binding stmt))

  let finalize cstmt =
    Success.stepthrough
      (fun stmt -> Success.finalize_safe stmt.statement)
      cstmt.code
        >>= function
        | Sqlite3.Rc.OK -> (Success.return ())
        | whatever -> Success.failwith_sqlite3 "CompiledStatement.finalize" whatever cstmt.handle
end


module ConcreteStatement =
struct
  type t = {
    sql: string;
    hash: int;
    binding: Binding.t;
    last_insert_rowid: int64 ref option;
  }

  let from_string sql = {
    sql;
    hash = Hashtbl.hash(sql);
    binding = Binding.empty;
    last_insert_rowid = None;
  }

  let equal s1 s2 =
    String.compare s1.sql s2.sql = 0

  let hash s =
    s.hash

  let apply ?rowid statement binding =
    { statement with binding; last_insert_rowid = rowid; }

  let pp_print pp s =
    let open Format in
    let pp_print_optref pp x =
      match x with
      | None -> fprintf pp "None"
      | Some(r) -> fprintf pp "Some(!%Ld)" !r
    in
    fprintf pp
      "@[<hv 1>ConcreteStatement.{@ \
       @[<hv 1>sql =@ %S@];@ \
       @[<hv 1>hash =@ %d@];@ \
       @[<hv 1>binding =@ %a@];@ \
       @[<hv 1>last_insert_row_id = %a@];@ \
       @]}"
      s.sql
      s.hash
      Binding.pp_print s.binding
      pp_print_optref s.last_insert_rowid
end

let pp_print_statement =
  ConcreteStatement.pp_print

module StatementTable =
  Hashtbl.Make(ConcreteStatement)

module Handle : sig
  type t
  val make : string -> t
  val release : t -> unit
  val prepare : ConcreteStatement.t -> t -> CompiledStatement.statement list Success.t
  val last_insert_rowid : t -> int64 Success.t
  val pp_print : Format.formatter -> t -> unit
end = struct
  let cache_sz = 100
  let retry_delay = 10
  let retry_n = 100

  type t = {
    cache : CompiledStatement.t StatementTable.t;
    filename: string;
    sqlitedb: Sqlite3.db;
  }

  let pp_print_cache_item ff (k, v) =
    let open Format in
    fprintf ff "@[<hv 1>%a,@ %a@]"
      ConcreteStatement.pp_print k
      CompiledStatement.pp_print v

  let pp_print_cache ff cache =
    let open Format in
    let flag = ref false in
    let loop k v =
      if !flag then fprintf ff ";@ ";
      flag := true;
      fprintf ff "%a" pp_print_cache_item (k, v)
    in
    fprintf ff "@[<hov 1>[";
    StatementTable.iter loop cache;
    fprintf ff "]@]"


  let pp_print pp handle =
    let open Format in
    fprintf pp
      "@[<hv 1>Handle.{@ \
       @[<hv 1>cache =@ %a@];@ \
       @[<hv 1>filename =@ %S@];@ \
       @[<hv 1>sqlitedb =@ <abstr>@ @[<hv 3>(* %s:@ %s *)@]@];@ \
       @]}"
      pp_print_cache handle.cache
      handle.filename
      (Sqlite3.Rc.to_string (Sqlite3.errcode handle.sqlitedb))
      (Sqlite3.errmsg handle.sqlitedb)

  let make filename = {
    cache = StatementTable.create cache_sz;
    filename;
    sqlitedb = Sqlite3.db_open ~mutex:`NO ~cache:`PRIVATE filename;
  }

  let finalize_cache handle =
    let open Success.Infix in
    let loop key statement m =
      m
      >>= fun () ->
      CompiledStatement.finalize statement
      >>= fun () ->
      Success.return(StatementTable.remove handle.cache key)
    in
    StatementTable.fold loop handle.cache (Success.return ())

  let release handle =
    let open Success.Infix in
    let rec retry n control =
      if control then
        Success.return ()
      else if n > 0 then begin
        Pervasives.ignore(Sqlite3.sleep retry_delay);
        Success.db_close handle.sqlitedb >>= retry (n-1)
      end else
        Success.errorf "closedb" "%s: Cannot close database '%s' (%s)."
          (Sqlite3.Rc.to_string (Sqlite3.errcode handle.sqlitedb))
          handle.filename
          (Sqlite3.errmsg handle.sqlitedb)
    in
    Success.unsafe_run begin
      finalize_cache handle
      >>= fun () -> Success.db_close handle.sqlitedb
      >>= retry retry_n
    end

  let prepare concrete handle =
    let open Success.Infix in
    let stmt =
      try Success.return(StatementTable.find handle.cache concrete)
      with Not_found -> begin
          CompiledStatement.make concrete.ConcreteStatement.sql handle.sqlitedb
          >>= fun stmt ->
          StatementTable.add handle.cache concrete stmt;
          Success.return stmt
        end
    in
    stmt >>= CompiledStatement.statements

  let last_insert_rowid handle =
    Success.last_insert_rowid handle.sqlitedb
end

let pp_print_handle =
  Handle.pp_print

include Success

type handle = Handle.t

type statement =
  ConcreteStatement.t

type binding =
  Binding.t


(* Execute statements *)

let maybe_store_last_insert_rowid concrete handle () =
  let open Success.Infix in
  match concrete.ConcreteStatement.last_insert_rowid with
  | None -> Success.return ()
  | Some(x) ->
      Handle.last_insert_rowid handle
      >>= function id -> x := id; Success.return()

let exec ?binding concrete handle =
  let open Success.Infix in
  let concrete =
    match binding with
    | Some(b) -> ConcreteStatement.apply concrete b
    | None -> concrete
  in
  let rec loop lst =
    match lst with
    | stmt :: tl ->
        CompiledStatement.exec
          concrete.ConcreteStatement.binding
          stmt
        >>= fun () -> loop tl
    | [] -> Success.return ()
  in
  Handle.prepare concrete handle
  >>= loop
  >>= maybe_store_last_insert_rowid concrete handle

let stream_of_slist m =
  let open Success.Infix in
  match Success.run m with
  | Success.Success(lst) -> S.of_list lst
  | Success.Error(_ as err) ->  S.from (fun _ -> Success.error err)

let query ?binding concrete handle =
  let open Success.Infix in
  let concrete =
    match binding with
    | Some(b) -> ConcreteStatement.apply concrete b
    | None -> concrete
  in
  Handle.prepare concrete handle
  |> stream_of_slist
  |> S.map (CompiledStatement.query concrete.ConcreteStatement.binding)
  |> S.concat


let one rows =
  let open Success.Infix in
  S.npeek 2 rows
  >>= function
  | [x] -> Success.return x
  | _ -> Success.errorm "one" "The stream has not exactly one row."

let maybe rows =
  S.get rows

let project p rows =
  let open Success.Infix in
  let remember = ref None in
  let get () =
    match !remember with
    | Some(x) -> remember := None; Success.return(Some x)
    | None -> S.get rows
  in
  let pullback x =
    remember := Some(x)
  in
  let rec get_fibre base fibre =
    get () >>= function
    | Some(x) ->
        let (b, y) = p x in
        if b = base then
          get_fibre base (y::fibre)
        else
          (pullback x; Success.return(Some(base, List.rev fibre)))
    | None -> Success.return (Some(base, List.rev fibre))
  in
  let f _ =
    get () >>= function
    | Some(x) ->
        let (b, y) = p x in
        get_fibre b [y]
    | None -> Success.return None
  in
  S.from f

let insert stream handle =
  Success.join begin
    S.fold
      (fun stmt m -> Success.bind m (fun () -> exec stmt handle))
      stream
      (Success.return())
  end

let statement sql =
  ConcreteStatement.from_string sql

let rowid_binding r =
  fun _ -> INT(!r)

let bindings p s =
  let f x =
    List.map (fun(key, get) -> key, get x) p
  in
  S.map f s

let bindings_apply ?rowid b stmt =
  S.map (ConcreteStatement.apply ?rowid stmt) b


(* The database file *)

let closedb handle =
  Handle.release handle

let opendb ?init filename =
  let initdb =
    match Sys.file_exists filename, init with
    | false, Some(sql) ->
        fun handle -> exec (statement sql) handle
    | _ ->
        fun _ -> return ()
  in
  let handle =
    Handle.make filename
  in
  try
    Success.unsafe_run (initdb handle);
    handle
  with exn ->
    (closedb handle; raise exn)

let withdb ?init filename f =
  let handle = opendb ?init filename in
  try
    let answer = (f handle) in
    (closedb handle; answer)
  with exn ->
    (closedb handle; raise exn)
