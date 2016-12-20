.SUFFIXES:
.SUFFIXES: .sc .o

OBJS =  ezd.o commands.o ginfo.o display.o window.o view.o drawing.o \
        graphic.o events.o

.sc.o:
	csc -ic $*.sc

check: ${OBJS} check.scm
	csc -o check check.scm ${OBJS}

clean:
	rm -f ${OBJS} check