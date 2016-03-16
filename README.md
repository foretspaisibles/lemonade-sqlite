# Monadic interface to sqlite

The **Lemonade Sqlite** project aims at providing high-level, easy to use,
monadic bindings for sqlite3 in OCaml.

[![Build Status](https://travis-ci.org/michipili/lemonade-sqlite.svg?branch=master)](https://travis-ci.org/michipili/lemonade-sqlite?branch=master)

## Features

- Provide detailed error descriptions and easy error management
  by performing database operations in a success monad.

- Query rows as a monadic stream. A monadic stream is
  similar to a normal stream but reading from the stream is performed
  in the success monad, thus conveying neatly error conditions to the
  application level.

- Insert rows read from a monadic stream. This allows to easily
  map complex data structures to the database rows scattered through
  several tables, because it easy to combine streams together.

- Optionally initialise the database with a SQL script when creating it.

These techniques are illustrated by the example
`example/BatchCollection.ml` implementing operations to read and write
items of *batch job journal* to a database.  In this example, a
journal of batch jobs, describing requests (table *batch_index*) the
details of the corresponding query jobs (table *batch_query*) and the
details of the computed answer (table *batch_answer*) is randomly
generated, written to the database and retrieved.


## Setup guide

It is easy to install **Lemonade Sqlite** using **opam** and its *pinning*
feature.  In a shell visiting the repository, say

```console
% autoconf
% opam pin add lemonade-sqlite .
```

It is also possible to install **Lemonade Sqlite** manually.
The installation procedure is based on the portable build system
[BSD Owl Scripts][bsdowl-home] written for BSD Make.

1. Verify that prerequisites are installed:
   - BSD Make
   - [BSD OWl][bsdowl-install]
   - OCaml
   - [Broken][broken-home]
   - [Mixture][mixture-home]
   - [Lemonade][lemonade-home]
   - GNU Autoconf

2. Get the source, either by cloning the repository or by exploding a
   [distribution tarball](releases).

3. Optionally run `autoconf` to produce a configuration script. This
   is only required if the script is not already present.

4. Run `./configure`, you can choose the installation prefix with
   `--prefix`.

5. Run `make build`.

6. Optionally run `make test` to test your build.

7. Finally run `make install`.

Depending on how **BSD Make** is called on your system, you may need to
replace `make` by `bsdmake` or `bmake` in steps 5, 6, and 7.
The **GNU Make** program usually give up the ghost, croaking
`*** missing separator. Stop.` when you mistakingly use it instead of
**BSD Make**.

Step 7 requires that you can `su -` if you are not already `root`.


Michael Grünewald in Bonn, on Mar 16, 2016

  [licence-url]:        http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html
  [licence-en]:         COPYING
  [licence-fr]:         COPYING-FR
  [bsdowl-home]:        https://github.com/michipili/bsdowl
  [bsdowl-install]:     https://github.com/michipili/bsdowl/wiki/Install
  [broken-home]:        https://github.com/michipili/broken
  [mixture-home]:       https://github.com/michipili/mixture
  [lemonade-home]:      https://github.com/michipili/lemonade
