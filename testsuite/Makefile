### Makefile -- Unit testing

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

PROGRAM=	unit_testing

SRCS=		test_Lemonade_sqlite.ml

SRCS+=		main.ml

OCAMLLFLAGS+=	-linkall

do-test: ${PROGRAM}
	${PROGRAM:tA}

install doc:
	${NOP}

.include "ocaml.prog.mk"
