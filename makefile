GLUT_FILES = scm-lib.scm opengl.scm opengl-header.scm glu.scm glu-header.scm glut.scm glut-header.scm

SPACE_INVADERS_FILES = engine.scm user-interface.scm 

PATH_TO_GAMBIT=/opt/gambit-c/current
GAMBIT_LIB=$(PATH_TO_GAMBIT)/lib
GAMBIT_INCLUDE=$(PATH_TO_GAMBIT)/include
GSC=gsc -:=$(PATH_TO_GAMBIT)
CC=gcc 
INCLUDE_OPTIONS=-I$(GAMBIT_INCLUDE)
LD_OPTIONS =-lglut -lgambc -lutil -L$(GAMBIT_LIB)

.SUFFIXES:
.SUFFIXES: .c .scm .o .o1
.PHONY: all clean shared-objects 

all: space-invaders #opengl-test opengl-test2

space-invaders: $(GLUT_FILES:.scm=.c) $(SPACE_INVADERS_FILES:.scm=.c) space-invaders_.c 
	$(CC) $(INCLUDE_OPTIONS) -o $@ $^ $(LD_OPTIONS)

space-invaders_.c: $(GLUT_FILES:.scm=.c) $(SPACE_INVADERS_FILES:.scm=.c)
	$(GSC) -o $@ -link $^ 



## Optionnal version which helps to see bottlenecks in code execution
profile: profile.c profile_.c
	$(CC) $(INCLUDE_OPTIONS) -o $@ $^ $(LD_OPTIONS)

profile_.c: $(GLUT_FILES:.scm=.o1) Question2.o1 Question2-ui.o1 profile.c
	$(GSC) -link profile.c

profile.c: statprof.scm profile.scm
	$(GSC) -c $*.scm



.scm.c:
	$(GSC) -c $*
#	$(GSC) -expansion -c $*


.scm.o1:
	$(GSC) -ld-options "-lglut" -debug-source -o $*.o1 $*.scm




opengl-test: $(GLUT_FILES:.scm=.c) opengl-test.c opengl-test_.c
	$(CC) $(INCLUDE_OPTIONS) -o $@ $^ $(LD_OPTIONS)

opengl-test_.c: $(GLUT_FILES:.scm=.c) opengl-test.c
	$(GSC) -link $^


opengl-test2: $(GLUT_FILES:.scm=.c) opengl-test2.c opengl-test2_.c
	$(CC) $(INCLUDE_OPTIONS) -o $@ $^ $(LD_OPTIONS)

opengl-test2_.c: $(GLUT_FILES:.scm=.c) opengl-test2.c
	$(GSC) -link $^



clean:
	rm -f $(GLUT_FILES:.scm=.c) $(SPACE_INVADERS_FILES:.scm=.c) *.o* opengl-test opengl-test2 space-invaders