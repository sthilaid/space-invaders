SPRITE_FILES = $(wildcard sprites/*.ppm) $(wildcard sprites/*.bmp)
FONT_FILES = $(wildcard fonts/*.ppm) $(wildcard fonts/*.scm)
DOC_FILES = $(wildcard doc/*.ppm)
SOUND_FILES = $(wildcard sounds/*.wav)

GL_FILES = opengl.scm glu.scm
UI_FILES = sdl-interface.scm sdl-user-interface.scm
DEVEL_FILES = $(GL_FILES) rbtree.scm scm-lib.scm scm-lib-macro.scm stats.scm ppm-reader.scm texture.scm sprite.scm font.scm user-interface-images.scm $(UI_FILES)
SPACE_INVADERS_FILES =  $(DEVEL_FILES) new-engine.scm

## compilers
GSC=$(PATH_TO_GAMBIT)/bin/gsc -:=$(PATH_TO_GAMBIT) -debug
CC=gcc

## Gambit-c
PATH_TO_GAMBIT=/opt/gambit-c
GAMBIT_LIB=$(PATH_TO_GAMBIT)/lib
GAMBIT_INCLUDE=$(PATH_TO_GAMBIT)/include

## Some scheme libraries paths
OOSYSYEM_PATH=$(HOME)/projet/maitrise/class
SCMLIB_PATH=$(HOME)/projet/maitrise/scm-lib
THRDSIM_PATH=$(HOME)/projet/maitrise/thread-simulation

## Default options
UI=sdl
OS=linux
VERSION=1.0


LD_OPTIONS_LIN = -lutil -lSDL -lSDL_mixer -lglut
LD_OPTIONS_COMMON =-L$(GAMBIT_LIB) -L$(GL_LIB) -L$(SDL_LIB) -L$(SDL_mixer_LIB) -lgambc 


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

INCLUDE_OPTIONS=-I$(GAMBIT_INCLUDE) -I$(GL_INCLUDE) $(ALL_SDL_INCLUDE)


.SUFFIXES:
.SUFFIXES: .c .scm .o .o1 .m
.PHONY: all clean shared-objects tarball welcome

all: welcome run-invaders 

devel: $(DEVEL_FILES:.scm=.o1) new-engine.scm
	gsi -:dar start.scm

run-invaders: $(SPACE_INVADERS_FILES:.scm=.o1)
	@echo "*** Compilation Finished ***"
	@echo
	@echo "Starting Space Invaders...."
	@echo
	gsi $(SPACE_INVADERS_FILES:.scm=.o1) -e '(main)'

## "included" macro dependant scheme source files
user-interface-images.c: user-interface-images.scm texture-macro.scm font-macro.scm scm-lib-macro.scm
	$(GSC) -c user-interface-images.scm 

user-interface.c: user-interface.scm scm-lib-macro.scm opengl-header.scm
	$(GSC) -c user-interface.scm 

sdl-user-interface.c: sdl-user-interface.scm scm-lib-macro.scm opengl-header.scm
	$(GSC) -c sdl-user-interface.scm 

new-engine.c: new-engine.scm thread-simulation-macro.scm class.scm thread-simulation.scm
	$(GSC) -c new-engine.scm

# rule used for 'make devel'
new-engine.scm: thread-simulation-macro.scm class.scm thread-simulation.scm

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

thread-simulation.scm: $(THRDSIM_PATH)/thread-simulation.scm
	cp $(THRDSIM_PATH)/thread-simulation.scm .

thread-simulation-macro.scm: $(THRDSIM_PATH)/thread-simulation-macro.scm
	cp $(THRDSIM_PATH)/thread-simulation-macro.scm .

.scm.o1:
	$(GSC) -cc-options "$(INCLUDE_OPTIONS)" -ld-options "$(LD_OPTIONS)" -debug-source -o $*.o1 $*.scm


## Welcome banner
 welcome:
	@echo "*** Currently using following paths ***"
	@echo
	@echo PATH_TO_GAMBIT=$(PATH_TO_GAMBIT)
	@echo PATH_TO_GL=$(PATH_TO_GL)
ifeq ($(UI), sdl)
	@echo PATH_TO_SDL=$(PATH_TO_SDL)
	@echo PATH_TO_SDL_mixer=$(PATH_TO_SDL_mixer)
endif
	@echo
	@echo "*** Beginning Compilation ***"

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
