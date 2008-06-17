
PATH_TO_GAMBIT=/opt/gambit-c/current
GAMBIT_LIB=$(PATH_TO_GAMBIT)/lib
GAMBIT_INCLUDE=$(PATH_TO_GAMBIT)/include

GSC=$(PATH_TO_GAMBIT)/bin/gsc -:=$(PATH_TO_GAMBIT) -debug
CC=gcc

GLUT_FILES = opengl.scm glu.scm 

SPRITE_FILES = $(wildcard sprites/*.ppm) $(wildcard sprites/*.bmp)
FONT_FILES = $(wildcard fonts/*.ppm) $(wildcard fonts/*.scm)
DOC_FILES = $(wildcard doc/*.ppm)
SOUND_FILES = $(wildcard sounds/*.wav)

SPACE_INVADERS_FILES =  scm-lib.scm rbtree.scm ppm-reader.scm event-simulation.scm texture.scm sprite.scm font.scm coroutine.scm engine.scm user-interface-images.scm 

UI=sdl
OS=linux

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
LD_OPTIONS_COMMON =-L$(GAMBIT_LIB) -L$(GL_LIB) -L$(SDL_LIB) -lgambc 
endif



ifeq ($(OS), mac)
# Paths not required for mac os if using the -framework options?
PATH_TO_GL=/System/Library/Frameworks/OpenGL.framework
GL_INCLUDE=$(PATH_TO_GL)/Headers
GL_LIB=$(PATH_TO_GL)/Libraries

GLUT_INCLUDE=/System/Library/Frameworks/GLUT.framework/Headers

PATH_TO_SDL=/System/Library/Frameworks/SDL.framework
SDL_INCLUDE=$(PATH_TO_SDL)/Headers
SDL_LIB=$(PATH_TO_SDL)

PATH_TO_SDL_devel=/System/Library/Frameworks/SDL.framework/SDL-devel-extras/SDLMain/NIBless

PATH_TO_SDL_mixer=/System/Library/Frameworks/SDL_mixer.framework
SDL_mixer_INCLUDE=$(PATH_TO_SDL_mixer)/Headers

ALL_SDL_INCLUDE=-I$(SDL_INCLUDE) -I$(SDL_mixer_INCLUDE) -I$(PATH_TO_SDL_devel)
LD_OPTIONS = $(LD_OPTIONS_COMMON) $(LD_OPTIONS_MAC)
endif

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

ALL_SDL_INCLUDE=-I$(SDL_INCLUDE) -I$(SDL_mixer_INCLUDE)
LD_OPTIONS = $(LD_OPTIONS_COMMON) $(LD_OPTIONS_WIN)
endif



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

ifeq ($(OS), mac)
space-invaders.exe: $(GLUT_FILES:.scm=.o) $(SPACE_INVADERS_FILES:.scm=.o) $(UI_FILES:.scm=.o) space-invaders_.o SDLMain.m
	$(CC) $(INCLUDE_OPTIONS) -o $@ $^ $(LD_OPTIONS)
else
space-invaders.exe: $(GLUT_FILES:.scm=.o) $(SPACE_INVADERS_FILES:.scm=.o) $(UI_FILES:.scm=.o) space-invaders_.o
	$(CC) $(INCLUDE_OPTIONS) -o $@ $^ $(LD_OPTIONS)
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

space-invaders_.c: $(GLUT_FILES:.scm=.c) $(SPACE_INVADERS_FILES:.scm=.c) $(UI_FILES:.scm=.c)
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

.m.o:
	$(CC) $(INCLUDE_OPTIONS) -c $*.m

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
	rm -f $(ALL_SCM:.scm=.c) *_.c *.o* space-invaders.exe *.tar.gz *.~*~
	$(MAKE) clean -C doc

tarball: makefile $(wildcard *.scm) $(SPRITE_FILES) $(FONT_FILES) $(DOC_FILES)
	tar cvzf space-invaders-src.tar.gz $(foreach file, $^, ../space-invaders/$(file))

release: space-invaders.exe $(SPRITE_FILES) $(FONT_FILES) $(SOUND_FILES)
ifeq ($(OS), win)
# 	tar cvzf space-invaders.tar.gz $(foreach file, $^ SDL.dll SDL_mixer.dll, ../space-invaders/$(file))
	zip -r space-invaders.zip $(foreach file, $^ SDL.dll SDL_mixer.dll, ../space-invaders/$(file))
else
	tar cvzf space-invaders.tar.gz $(foreach file, $^, ../space-invaders/$(file))
endif
