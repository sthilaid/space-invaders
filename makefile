##											 Space-invaders makefile
##
## usage: The makefile possess 2 main variables OS and UI which
## controls respectively the operating system used (win, mac, linux)
## and the user interface used (glut or sdl).
##
## There are also many PATH_TO_XYZ variables that encapsulates the
## dependent library paths. These PATH_TO variables are usually
## accompanied by XYZ_INCLUDE and XYZ_LIB variables which gives the
## location of the library file and header files associated with
## XYZ. Setting only the PATH_TO_XYZ should work in most cases, but
## setting directly the XYZ_LIB and XYZ_INCLUDE variables gives finer
## granularity over the compilation and is required to cross-compile
## the program.
##
## some general usage exemples:
##
## make OS=linux UI=sdl PATH_TO_SDL_mixer=$HOME/SDL_mixer
## make OS=linux UI=glut
## make OS=win UI=sdl PATH_TO_GAMBIT=$HOME/gambit-c-win/current

## Source files
SPRITE_FILES = $(wildcard sprites/*.ppm) $(wildcard sprites/*.bmp)
FONT_FILES = $(wildcard fonts/*.ppm) $(wildcard fonts/*.scm)
DOC_FILES = $(wildcard doc/*.ppm)
SOUND_FILES = $(wildcard sounds/*.wav)

GL_FILES = opengl.scm glu.scm 
SPACE_INVADERS_FILES =  scm-lib.scm scm-lib-macro.scm stats.scm ppm-reader.scm thread-simulation.scm texture.scm sprite.scm font.scm engine.scm user-interface-images.scm 


## compilers
GSC=$(PATH_TO_GAMBIT)/bin/gsc -:=$(PATH_TO_GAMBIT) -debug 
CC=gcc

## Gambit-c
PATH_TO_GAMBIT=/opt/gambit-c/current
GAMBIT_LIB=$(PATH_TO_GAMBIT)/lib
GAMBIT_INCLUDE=$(PATH_TO_GAMBIT)/include

## Some scheme libraries paths
OOSYSYEM_PATH=$(HOME)/diro/ift6232/class
SCMLIB_PATH=$(HOME)/projet/maitrise/scm-lib


## Default options
UI=sdl
OS=linux
VERSION=1.0

## UI dependent variables
ifeq ($(UI), glut)
UI_FILES = glut.scm user-interface.scm
LD_OPTIONS_LIN = -lutil -lglut
LD_OPTIONS_MAC = -framework GLUT -lobjc -framework OpenGL
LD_OPTIONS_WIN = -lglut32 -lglu32 -lopengl32 -lws2_32 -mwindows
LD_OPTIONS_COMMON =-L$(GAMBIT_LIB) -L$(GL_LIB) -lgambc

# SDL used in general:
else
UI_FILES = sdl-interface.scm sdl-user-interface.scm
LD_OPTIONS_LIN = -lutil -lSDL -lSDL_mixer -lglut
LD_OPTIONS_MAC = -framework SDL -framework SDL_mixer -lobjc -framework OpenGL -framework Cocoa
LD_OPTIONS_WIN = -lSDL -lSDL_mixer -lglu32 -lopengl32 -lws2_32 -mwindows
LD_OPTIONS_COMMON =-L$(GAMBIT_LIB) -L$(GL_LIB) -L$(SDL_LIB) -L$(SDL_mixer_LIB) -lgambc 
endif


############### OS dependent variables ###############

## MAC OSX
ifeq ($(OS), mac)
# Paths not required for mac os if using the -framework options?
PATH_TO_GL=/System/Library/Frameworks/OpenGL.framework
GL_INCLUDE=$(PATH_TO_GL)/Headers
GL_LIB=$(PATH_TO_GL)/Libraries

GLUT_INCLUDE=/System/Library/Frameworks/GLUT.framework/Headers

PATH_TO_SDL=/System/Library/Frameworks/SDL.framework
SDL_INCLUDE=$(PATH_TO_SDL)/Headers
SDL_LIB=$(PATH_TO_SDL)

PATH_TO_SDL_devel=/System/Library/Frameworks/SDL.framework/SDL-devel-extras

PATH_TO_SDL_mixer=/System/Library/Frameworks/SDL_mixer.framework
SDL_mixer_INCLUDE=$(PATH_TO_SDL_mixer)/Headers
SDL_mixer_LIB=$(PATH_TO_SDL_mixer)

ALL_SDL_INCLUDE=-I$(SDL_INCLUDE) -I$(SDL_mixer_INCLUDE) -I$(PATH_TO_SDL_devel)
LD_OPTIONS = $(LD_OPTIONS_COMMON) $(LD_OPTIONS_MAC)
endif


## Windows
ifeq ($(OS), win)
PATH_TO_GL=/mingw
GL_INCLUDE=$(PATH_TO_GL)/include/GL
GL_LIB=$(PATH_TO_GL)/lib

GLUT_INCLUDE=/mingw/include/GL

PATH_TO_SDL=/usr/local
SDL_INCLUDE=$(PATH_TO_SDL)/include/SDL
SDL_LIB=$(PATH_TO_SDL)/lib
SDL_BIN=$(PATH_TO_SDL)/bin

PATH_TO_SDL_mixer=/usr/local
SDL_mixer_INCLUDE=$(PATH_TO_SDL_mixer)/include/SDL
SDL_mixer_LIB=$(PATH_TO_SDL_mixer)/lib

ALL_SDL_INCLUDE=-I$(SDL_INCLUDE) -I$(SDL_mixer_INCLUDE)
LD_OPTIONS = $(LD_OPTIONS_COMMON) $(LD_OPTIONS_WIN)
endif


## Linux
ifeq ($(OS), linux)
PATH_TO_GL=/usr
GL_INCLUDE=$(PATH_TO_GL)/include/GL
GL_LIB=$(PATH_TO_GL)/lib

GLUT_INCLUDE=/usr/include/GL

PATH_TO_SDL=/usr
SDL_INCLUDE=$(PATH_TO_SDL)/include/SDL
SDL_LIB=$(PATH_TO_SDL)/lib

PATH_TO_SDL_mixer=/usr
SDL_mixer_INCLUDE=$(PATH_TO_SDL_mixer)/include/SDL
SDL_mixer_LIB=$(PATH_TO_SDL_mixer)/lib

ALL_SDL_INCLUDE=-I$(SDL_INCLUDE) -I$(SDL_mixer_INCLUDE)
LD_OPTIONS = $(LD_OPTIONS_COMMON) $(LD_OPTIONS_LIN)
endif

ifeq ($(UI), glut)
INCLUDE_OPTIONS=-I$(GAMBIT_INCLUDE) -I$(GL_INCLUDE) -I$(GLUT_INCLUDE)
else
INCLUDE_OPTIONS=-I$(GAMBIT_INCLUDE) -I$(GL_INCLUDE) $(ALL_SDL_INCLUDE)
endif



.SUFFIXES:
.SUFFIXES: .c .scm .o .o1 .m
.PHONY: all clean shared-objects tarball welcome

all: welcome space-invaders.exe dll

dll:
ifeq ($(OS), win)
ifeq ($(UI), sdl)
	cp $(SDL_BIN)/SDL.dll $(SDL_BIN)/SDL_mixer.dll .
endif
endif


## the SDL mac version must be compiled with the SDLMain.m file
## contained in the SDL devel package for mac osx.
ifeq ($(OS), mac)
ifeq ($(UI), sdl)
space-invaders.exe: $(GL_FILES:.scm=.o) $(SPACE_INVADERS_FILES:.scm=.o) $(UI_FILES:.scm=.o) space-invaders_.o $(PATH_TO_SDL_devel)/SDLMain/NIBless/SDLMain.m
	$(CC) $(INCLUDE_OPTIONS) -o $@ $^ $(LD_OPTIONS)
endif
else
space-invaders.exe: $(GL_FILES:.scm=.o) $(SPACE_INVADERS_FILES:.scm=.o) $(UI_FILES:.scm=.o) space-invaders_.o 
	$(CC) $(INCLUDE_OPTIONS) -o $@ $(GL_FILES:.scm=.o) $(SPACE_INVADERS_FILES:.scm=.o) $(UI_FILES:.scm=.o) space-invaders_.o $(LD_OPTIONS)
endif

## only in mac osx, the main function must be renaimed to SDL_main.
## The "right" way is to add #include <SDL.h> in the file where main is defined
## but since it is difficult to add it to the link file, we chose to define it 
## by ourselves. The commented code can be used if the -Dmain=SDL_main stops working
## at some point
ifeq ($(OS), mac)
ifeq ($(UI), sdl)
space-invaders_.o: space-invaders_.c
#	cat $^ > link-temp
#	echo "#include <SDL.h>" > $^
#	cat link-temp >> $^
#	rm link-temp
	$(CC) $(INCLUDE_OPTIONS) -c $^ -Dmain=SDL_main
endif
endif

## Scheme link file
space-invaders_.c: $(GL_FILES:.scm=.c) $(SPACE_INVADERS_FILES:.scm=.c) $(UI_FILES:.scm=.c)
	$(GSC) -o $@ -link $^


## "included" macro dependant scheme source files
user-interface-images.c: user-interface-images.scm texture-macro.scm font-macro.scm scm-lib-macro.scm
	$(GSC) -c user-interface-images.scm 

user-interface.c: user-interface.scm scm-lib-macro.scm opengl-header.scm
	$(GSC) -c user-interface.scm 

sdl-user-interface.c: sdl-user-interface.scm scm-lib-macro.scm opengl-header.scm
	$(GSC) -c sdl-user-interface.scm 

engine.c: engine.scm thread-simulation-macro.scm class.scm
	$(GSC) -c engine.scm


# Opengl interface interdependance
opengl.c: opengl.scm opengl-header.scm
	$(GSC) -c opengl.scm

glu.c: glu.scm glu-header.scm
	$(GSC) -c glu.scm

glut.c: glut.scm glut-header.scm
	$(GSC) -c glut.scm


# External Scheme library dependencies
class.scm: $(OOSYSYEM_PATH)/class.scm
	cp $(OOSYSYEM_PATH)/class.scm .

scm-lib.scm: $(SCMLIB_PATH)/scm-lib.scm
	cp $(SCMLIB_PATH)/scm-lib.scm .

scm-lib-macro.scm: $(SCMLIB_PATH)/scm-lib-macro.scm
	cp $(SCMLIB_PATH)/scm-lib-macro.scm .


## General build instructions
.m.o:
	$(CC) $(INCLUDE_OPTIONS) -c $*.m

.c.o:
	$(CC) $(INCLUDE_OPTIONS) -c $*.c

.scm.c: 
	$(GSC) -c $*
#	$(GSC) -expansion -c $*


.scm.o1:
	$(GSC) -ld-options "-lglut" -debug-source -o $*.o1 $*.scm


## Welcome banner
 welcome:
	@echo "*** Global Variables ***"
	@echo
	@echo OS=$(OS)
	@echo UI=$(UI)
	@echo
	@echo "*** Currently using following paths ***"
	@echo
	@echo PATH_TO_GAMBIT=$(PATH_TO_GAMBIT)
	@echo PATH_TO_GL=$(PATH_TO_GL)
ifeq ($(UI), sdl)
	@echo PATH_TO_SDL=$(PATH_TO_SDL)
	@echo PATH_TO_SDL_mixer=$(PATH_TO_SDL_mixer)
ifeq ($(OS), mac)
	@echo PATH_TO_SDL_devel=$(PATH_TO_SDL_devel)
endif
endif
	@echo
	@echo "*** Beginning compilation ***"

ALL_SCM = $(wildcard *.scm)
clean:
	rm -f $(ALL_SCM:.scm=.c) *_.c *.o* space-invaders.exe *.tar.gz *.tgz *.~*~ *.zip
  # external libs
	rm -f class.scm scm-lib.scm scm-lib-macro.scm
	$(MAKE) clean -C doc

tarball: makefile README $(wildcard *.scm) $(SPRITE_FILES) $(FONT_FILES) $(DOC_FILES) $(SOUND_FILES)
	tar cvzf space-invaders-src-v$(VERSION).tgz $(foreach file, $^, ../space-invaders/$(file))

release: space-invaders.exe $(SPRITE_FILES) $(FONT_FILES) $(SOUND_FILES)
ifeq ($(OS), win)
# 	tar cvzf space-invaders.tar.gz $(foreach file, $^ SDL.dll SDL_mixer.dll, ../space-invaders/$(file))
	zip -r space-invaders-$(OS).zip $(foreach file, $^ SDL.dll SDL_mixer.dll, ../space-invaders/$(file))
else
	tar cvzf space-invaders-$(OS).tar.gz $(foreach file, $^, ../space-invaders/$(file))
endif
