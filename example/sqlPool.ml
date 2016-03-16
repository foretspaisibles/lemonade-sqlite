(* SqlPool - Batch job journal SQL strings

Lemonade Sqlite (https://github.com/michipili/lemonade-sqlite)
This file is part of Lemonade Sqlite

Copyright © 2016 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

let init = "\
DROP TABLE IF EXISTS batch_index;
CREATE TABLE batch_index (
  id INTEGER PRIMARY KEY,
  received_at VARCHAR(24),
  completed_at VARCHAR(24),
  status INT
);
DROP TABLE IF EXISTS batch_query;
CREATE TABLE batch_query (
  id INTEGER PRIMARY KEY,
  batch_id INTEGER,
  timestamp VARCHAR(24),
  tablename TEXT,
  key VARCHAR(12)
);
DROP TABLE IF EXISTS batch_response;
CREATE TABLE batch_response (
  id INTEGER PRIMARY KEY,
  batch_id INTEGER,
  timestamp VARCHAR(24),
  tablename TEXT,
  key VARCHAR(12),
  item_n INTEGER,
  duration FLOAT
);
DROP TABLE IF EXISTS batch_duration;
CREATE TABLE batch_duration (
  batch_id INTEGER,
  duration FLOAT
);
DROP VIEW IF EXISTS analyse_query;
CREATE VIEW analyse_query AS
SELECT
    batch_query.batch_id AS batch_id,
    batch_query.tablename AS tablename,
    batch_query.key AS key,
    item_n,
    duration
  FROM batch_query INNER JOIN batch_response
 WHERE batch_query.batch_id = batch_response.batch_id
   AND batch_query.tablename = batch_response.tablename;
DROP VIEW IF EXISTS analyse_batch;
CREATE VIEW analyse_batch AS
SELECT
    batch_duration.batch_id,
    tablename,
    item_n,
    analyse_query.duration AS response_duration,
    batch_duration.duration AS batch_duration
  FROM analyse_query INNER JOIN batch_duration
 WHERE analyse_query.batch_id = batch_duration.batch_id;
DROP VIEW IF EXISTS persistance_query;
CREATE VIEW persistance_query AS
SELECT
  batch_query.batch_id AS batch_id,
  batch_query.tablename AS tablename,
  batch_query.key AS key,
  batch_query.timestamp AS query_timestamp,
  batch_response.timestamp AS response_timestamp,
  batch_response.item_n AS response_item_n,
  batch_response.duration AS response_duration
FROM batch_query LEFT JOIN batch_response
  WHERE batch_query.batch_id = batch_response.batch_id
  AND batch_query.tablename = batch_response.tablename
  AND batch_query.key = batch_response.key;
DROP VIEW IF EXISTS persistance_batch;
CREATE VIEW persistance_batch AS
SELECT
  *
FROM batch_index LEFT JOIN persistance_query
  WHERE batch_index.id = persistance_query.batch_id;
"

let insert_batch_index = "\
INSERT INTO batch_index (received_at, completed_at, status)
  VALUES($received_at, $completed_at, $status)
"

let insert_batch_query = "\
INSERT INTO batch_query (batch_id, timestamp, tablename, key)
  VALUES($batch_id, $timestamp, $tablename, $key)
"

let insert_batch_response = "\
INSERT INTO batch_response (batch_id, timestamp, tablename, key, item_n, duration)
VALUES($batch_id, $timestamp, $tablename, $key, $item_n, $duration)
"

let insert_batch_duration = "\
INSERT INTO batch_duration
  VALUES($batch_id, $duration)
"

let query_contents = "\
SELECT * FROM persistance_batch
"
