GLUT_FILES = opengl.scm opengl-header.scm glu.scm glu-header.scm glut.scm glut-header.scm

GRAPHICS_FILES = $(wildcard sprites/*.ppm)

SPACE_INVADERS_FILES =  scm-lib.scm rbtree.scm ppm-reader.scm event-simulation.scm texture.scm sprite.scm font.scm coroutine.scm engine.scm user-interface-images.scm user-interface.scm 

PATH_TO_GAMBIT=/opt/gambit-c/current
GAMBIT_LIB=$(PATH_TO_GAMBIT)/lib
GAMBIT_INCLUDE=$(PATH_TO_GAMBIT)/include
GSC=gsc -:=$(PATH_TO_GAMBIT) -debug
CC=gcc
INCLUDE_OPTIONS=-I$(GAMBIT_INCLUDE)
LD_OPTIONS =-lglut -lgambc -lutil -L$(GAMBIT_LIB)

.SUFFIXES:
.SUFFIXES: .c .scm .o .o1
.PHONY: all clean shared-objects tarball welcome

all: welcome space-invaders 

space-invaders: $(GLUT_FILES:.scm=.o) $(SPACE_INVADERS_FILES:.scm=.o) space-invaders_.o 
	$(CC) $(INCLUDE_OPTIONS) -o $@ $^ $(LD_OPTIONS)

space-invaders_.c: $(GLUT_FILES:.scm=.c) $(SPACE_INVADERS_FILES:.scm=.c)
	$(GSC) -o $@ -link $^ 


user-interface.c: user-interface.scm scm-lib-macro.scm texture-macro.scm font-macro.scm sprite-macro.scm opengl-header.scm
	$(GSC) -c user-interface.scm 

engine.c: engine.scm scm-lib.scm event-simulation.scm ppm-reader.scm coroutine.scm
	$(GSC) -c engine.scm

.c.o:
	$(CC) $(INCLUDE_OPTIONS) -c $*.c

.scm.c: 
	$(GSC) -c $*
#	$(GSC) -expansion -c $*


.scm.o1:
	$(GSC) -ld-options "-lglut" -debug-source -o $*.o1 $*.scm

welcome:
ifeq ($(PATH_TO_GAMBIT), /opt/gambit-c/current)
	@echo Please set the PATH_TO_GAMBIT variable to your gambit\'s current installation path.
	@echo ex: make PATH_TO_GAMBIT=/opt/gambit/current
endif
	@echo

ALL_SCM = $(wildcard *.scm)
clean:
	rm -f $(ALL_SCM:.scm=.c) *_.c *.o* space-invaders *.tar.gz

tarball: $(wildcard *.scm) $(GRAPHICS_FILES) makefile 
	tar cvzf space-invaders.tar.gz $(foreach file, $^, ../space-invaders/$(file))


