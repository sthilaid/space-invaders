GLUT_FILES = scm-lib.scm opengl.scm opengl-header.scm glu.scm glu-header.scm glut.scm glut-header.scm

GRAPHICS_FILES = bitmaps.c

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

user-interface.c: user-interface.scm $(GRAPHICS_FILES)
	$(GSC) -c user-interface.scm

.scm.c:
	$(GSC) -c $*
#	$(GSC) -expansion -c $*


.scm.o1:
	$(GSC) -ld-options "-lglut" -debug-source -o $*.o1 $*.scm


clean:
	rm -f $(GLUT_FILES:.scm=.c) $(SPACE_INVADERS_FILES:.scm=.c) *.o* opengl-test opengl-test2 space-invaders

