(* Timestamp - Working with timestamps

Lemonade Sqlite (https://github.com/michipili/lemonade-sqlite)
This file is part of Lemonade Sqlite

Copyright © 2016 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Unix
open Printf
open Scanf

let to_time timestamp =
  let convert year month day hour minute seconds =
    let unixtm = {
      tm_sec = 0;
      tm_min = minute;
      tm_hour = hour;
      tm_mday = day;
      tm_mon = month;
      tm_year = year - 1900;
      tm_wday = 0;
      tm_yday = 0;
      tm_isdst = false;
    } in
    let (secs, _) = Unix.mktime unixtm in
    secs +. seconds
  in
  sscanf timestamp "%d-%d-%dT%d:%d:%fZ" convert

let of_time time =
  let tm = gmtime time in
  let sec = time -. fst(mktime ({(localtime time) with tm_sec = 0})) in
  sprintf "%04d-%02d-%02dT%02d:%02d:%06.3fZ"
    (1900 + tm.tm_year)
    (1 + tm.tm_mon)
    tm.tm_mday tm.tm_hour tm.tm_min sec

let now () =
  of_time (gettimeofday())

let measure_interval timestart timestop =
  let a = to_time timestart in
  let b = to_time timestop in
  b -. a
