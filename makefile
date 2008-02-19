GLUT_FILES = opengl.scm opengl-header.scm glu.scm glu-header.scm glut.scm glut-header.scm

GSC=gambit-compiler
CC=gcc
INCLUDE_OPTIONS=
LD_OPTIONS = -lglut -lgambc

.SUFFIXES:
.SUFFIXES: .c .scm .o
.PHONY: all clean

all: opengl-test opengl-test2

opengl-test: $(GLUT_FILES:.scm=.c) opengl-test.c opengl-test_.c
	$(CC) $(INCLUDE_OPTIONS) -o $@ $^ $(LD_OPTIONS)

opengl-test_.c: $(GLUT_FILES:.scm=.c) opengl-test.c
	$(GSC) -link $^


opengl-test2: $(GLUT_FILES:.scm=.c) opengl-test2.c opengl-test2_.c
	$(CC) $(INCLUDE_OPTIONS) -o $@ $^ $(LD_OPTIONS)

opengl-test2_.c: $(GLUT_FILES:.scm=.c) opengl-test2.c
	$(GSC) -link $^



.scm.c:
	$(GSC) -c $*

clean:
	rm -f *.c *.o* opengl-test opengl-test2