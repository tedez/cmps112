# $Id: Makefile,v 1.15 2014-11-17 15:28:57-08 - - $

MKFILE    = Makefile
NOINCLUDE = ci clean spotless
NEEDINCL  = ${filter ${NOINCLUDE}, ${MAKECMDGOALS}}
SUBMAKE   = ${MAKE} --no-print-directory

#SOURCE    = bigint.mli bigint.ml maindc.ml scanner.mll
SOURCE	  = maindc.ml
# SOURCE	  = foo.ml
ALLSRC    = ${SOURCE} dc.ml ${MKFILE}
#OBJCMO    = bigint.cmo scanner.cmo maindc.cmo
OBJCMO 	  = maindc.cmo
# OBJCMO	  = foo.cmo
OBJCMI    = ${patsubst %.cmo, %.cmi, ${OBJCMO}}
CAMLRUN   = ocamldc

all : ${CAMLRUN}

${CAMLRUN} : ${OBJCMO} ${OBJCMI}
	ocamlc ${OBJCMO} -o ${CAMLRUN}

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

%.ml : %.mll
	ocamllex $<

clean :
	- rm ${OBJCMO} ${OBJCMI} scanner.ml

spotless : clean
	- rm ${CAMLRUN} ${LISTING} ${LISTING:.ps=.pdf}

ci : ${RCSFILES}
	cid + ${ALLSRC}
	checksource ${ALLSRC}

deps : ${SOURCE}
	ocamldep ${SOURCE} 

${DEPSFILE} :
	@ touch ${DEPSFILE}
	${SUBMAKE} deps

lis : ${ALLSRC}
	mkpspdf ${LISTING} ${ALLSRC} 

again :
	${SUBMAKE} spotless ci deps
	${SUBMAKE} all lis

# ifeq (${NEEDINCL}, )
# include ${DEPSFILE}
# endif

.PRECIOUS : scanner.ml
