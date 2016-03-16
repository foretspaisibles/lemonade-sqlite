### Makefile -- Project Lemonade Sqlite

# Lemonade Sqlite (https://github.com/michipili/lemonade-sqlite)
# This file is part of Lemonade Sqlite
#
# Copyright © 2016 Michael Grünewald
#
# This file must be used under the terms of the CeCILL-B.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt

PACKAGE=		lemonade-sqlite
VERSION=		0.1.0
OFFICER=		michipili@gmail.com

MODULE=			ocaml.lib:src
MODULE+=		ocaml.prog:testsuite
MODULE+=		ocaml.prog:example
MODULE+=		ocaml.meta:meta
MODULE+=		ocaml.manual:manual

EXTERNAL=		ocaml.findlib:broken
EXTERNAL+=		ocaml.findlib:mixture
EXTERNAL+=		ocaml.findlib:lemonade
EXTERNAL+=		ocaml.findlib:sqlite3

USES+=			ocamldoc

.include "generic.project.mk"

### End of file `Makefile'
