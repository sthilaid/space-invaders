GLUT_FILES = opengl.scm glu.scm  glut.scm 

SPRITE_FILES = $(wildcard sprites/*.ppm)
FONT_FILES = $(wildcard fonts/*.ppm)
DOC_FILES = $(wildcard doc/*.ppm)

SPACE_INVADERS_FILES =  scm-lib.scm rbtree.scm ppm-reader.scm event-simulation.scm texture.scm sprite.scm font.scm coroutine.scm engine.scm user-interface-images.scm user-interface.scm 

PATH_TO_GAMBIT=/opt/gambit-c/current
GAMBIT_LIB=$(PATH_TO_GAMBIT)/lib
GAMBIT_INCLUDE=$(PATH_TO_GAMBIT)/include

GSC=$(PATH_TO_GAMBIT)/bin/gsc -:=$(PATH_TO_GAMBIT) -debug
CC=gcc

OS=linux

LD_OPTIONS_COMMON =-L$(GAMBIT_LIB) -L$(GL_LIB) -lgambc 
LD_OPTIONS_LIN = -lutil -lglut
LD_OPTIONS_MAC = -framework GLUT -lobjc -framework OpenGL
LD_OPTIONS_WIN = -lglut32 -lglu32 -lopengl32 -lws2_32 -mwindows

ifeq ($(OS), mac)
# Paths not required for mac os if using the -framework options?
GL_INCLUDE=/System/Library/Frameworks/OpenGL.framework/Headers
GL_LIB=/System/Library/Frameworks/OpenGL.framework/Libraries
GLUT_INCLUDE=/System/Library/Frameworks/GLUT.framework/Headers
LD_OPTIONS = $(LD_OPTIONS_COMMON) $(LD_OPTIONS_MAC)
endif

ifeq ($(OS), win)
GL_INCLUDE=/mingw/include/GL
GL_LIB=/mingw/lib
GLUT_INCLUDE=/mingw/include/GL
LD_OPTIONS = $(LD_OPTIONS_COMMON) $(LD_OPTIONS_WIN)
endif

ifeq ($(OS), linux)
GL_INCLUDE=/usr/include/GL
GL_LIB=/usr/lib
GLUT_INCLUDE=/usr/include/GL
LD_OPTIONS = $(LD_OPTIONS_COMMON) $(LD_OPTIONS_LIN)
endif

INCLUDE_OPTIONS=-I$(GAMBIT_INCLUDE) -I$(GL_INCLUDE) -I$(GLUT_INCLUDE)

.SUFFIXES:
.SUFFIXES: .c .scm .o .o1
.PHONY: all clean shared-objects tarball welcome

all: welcome space-invaders

space-invaders: $(GLUT_FILES:.scm=.o) $(SPACE_INVADERS_FILES:.scm=.o) space-invaders_.o
ifeq ($(OS), win)
	$(CC) $(INCLUDE_OPTIONS) -o $@ $^ $(LD_OPTIONS)
else
	$(CC) $(INCLUDE_OPTIONS) -o $@ $^ $(LD_OPTIONS)
endif

space-invaders_.c: $(GLUT_FILES:.scm=.c) $(SPACE_INVADERS_FILES:.scm=.c)
	$(GSC) -o $@ -link $^ 

user-interface-images.c: user-interface-images.scm texture-macro.scm font-macro.scm scm-lib-macro.scm
	$(GSC) -c user-interface-images.scm 

user-interface.c: user-interface.scm scm-lib-macro.scm opengl-header.scm
	$(GSC) -c user-interface.scm 

engine.c: engine.scm event-simulation-macro.scm
	$(GSC) -c engine.scm

# Opengl interface interdependance
opengl.c: opengl.scm opengl-header.scm
	$(GSC) -c opengl.scm

glu.c: glu.scm glu-header.scm
	$(GSC) -c glu.scm

glut.c: glut.scm glut-header.scm
	$(GSC) -c glut.scm


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
	rm -f $(ALL_SCM:.scm=.c) *_.c *.o* space-invaders *.tar.gz *.~*~
	$(MAKE) clean -C doc

tarball: makefile $(wildcard *.scm) $(SPRITE_FILES) $(FONT_FILES) $(DOC_FILES)
	tar cvzf space-invaders.tar.gz $(foreach file, $^, ../space-invaders/$(file))


