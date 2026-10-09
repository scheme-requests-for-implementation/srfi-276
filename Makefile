.POSIX:
.PHONY: test

include config.makefile

test: gambit-lib/srfi/276.o1 gambit-contrib/srfi/252.o1
	gsi gambit-lib/ gambit-contrib/ tests/run.scm

gambit-lib/srfi/276.o1: gambit-lib/srfi/276.sld \
                        gambit-lib/srfi/144.special.scm \
                        lib/srfi/276.utils.scm \
                        lib/srfi/276.fl-not-equal.scm \
                        lib/srfi/276.constants.scm \
                        lib/srfi/276.total.scm \
                        lib/srfi/276.maxmin.scm \
                        lib/srfi/276.exp.scm \
                        lib/srfi/276.fllog.scm \
                        lib/srfi/276.compound.scm \
                        lib/srfi/276.rsqrt.scm \
                        lib/srfi/276.trigpi.scm \
                        lib/srfi/276.inverse-trigpi.scm
	${GSC} -module-ref 'srfi/276' -o gambit-lib/srfi/276.o1 gambit-lib/srfi/276.sld

gambit-contrib/srfi/194.o1: gambit-contrib/srfi/194.sld \
                            gambit-contrib/srfi/194-impl.scm \
                            gambit-contrib/srfi/zipf-zri.scm \
                            gambit-contrib/srfi/sphere.scm
	${GSC} -module-ref 'srfi/194' -o gambit-contrib/srfi/194.o1 gambit-contrib/  gambit-contrib/srfi/194.sld

gambit-contrib/srfi/252.o1: gambit-contrib/srfi/252.sld \
                            gambit-contrib/srfi/194.o1
	${GSC} -module-ref 'srfi/252' -o gambit-contrib/srfi/252.o1 gambit-contrib/ gambit-contrib/srfi/252.sld
