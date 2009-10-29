##############################################################################
## utilities
##############################################################################
add-presufix = $(foreach f, $(3), $(1)$(f)$(2))

# will only copy the used depency files. This implies that the
# recursive dependencies must be added to this project's depencies.
define define-dependency
DEPENDENCIES += $(1)
$(addprefix $$(SRC_PATH)/,$(2)) : setup-$(1)
setup-$(1):
	 mkdir -p $$(EXTERNAL_LIBS)
 ifeq "$$(wildcard $$(EXTERNAL_LIBS)/$(1))" ""
	 cd $$(EXTERNAL_LIBS) && git clone $$($(1)-PATH)
 endif
	 cd $$(EXTERNAL_LIBS)/$(1) && git pull
	 cd $$(EXTERNAL_LIBS)/$(1)/src/ && rsync -c $(2) ../../../$$(SRC_PATH)/
endef

##############################################################################
## project paths
##############################################################################
PREFIX=.
SRC_PATH=src
INCLUDE_PATH=$(PREFIX)/include
LIB_PATH=$(PREFIX)/lib
EXTERNAL_LIBS=$(PREFIX)/external-libs

##############################################################################
## Projet files
##############################################################################
SPRITE_FILES = $(wildcard sprites/*.ppm) $(wildcard sprites/*.bmp)
FONT_FILES = $(wildcard fonts/*.ppm) $(wildcard fonts/*.scm)
SOUND_FILES = $(wildcard sounds/*.wav)

INCLUDE_FILES = debug-declarations.scm release-declarations.scm \
	              scm-lib_.scm class.scm class_.scm  \
                opengl_.scm glu_.scm texture_.scm sprite_.scm font_.scm \
                class_.scm thread-simulation_.scm match.scm
LIB_FILES = statprof.o1 \
            scm-lib.o1 opengl.o1 glu.o1 ppm-reader.o1 texture.o1 sprite.o1 \
            font.o1 sdl-interface.o1 rbtree.o1 thread-simulation.o1 \
            user-interface-images.o1 engine.o1 sdl-user-interface.o1 
GAME_FILES = $(LIB_FILES)
COMPILED_FILES = opengl.o1 glu.o1 texture.o1 sprite.o1 sdl-interface.o1 \
                 font.o1 user-interface-images.o1

TARGET=debug
debug_DECLARATIONS=../include/debug-declarations.scm
release_DECLARATIONS=../include/release-declarations.scm

##############################################################################
## compilers and interpreters
##############################################################################
GSI=$(PATH_TO_GAMBIT)/bin/gsi -:=$(PATH_TO_GAMBIT),dar
GSC=$(PATH_TO_GAMBIT)/bin/gsc -:=$(PATH_TO_GAMBIT),dar -debug -prelude '(include "$($(TARGET)_DECLARATIONS)")'
cc=gcc

## Gambit-c location
PATH_TO_GAMBIT=/opt/gambit-c
GAMBIT_LIB=$(PATH_TO_GAMBIT)/lib
GAMBIT_INCLUDE=$(PATH_TO_GAMBIT)/include

##############################################################################
## Some scheme libraries git repos
##############################################################################

# class-PATH=git://github.com/sthilaid/class.git
# thread-simulation-PATH=git://github.com/sthilaid/thread-simulation.git
# scm-lib-PATH=git://github.com/sthilaid/scm-lib.git
# open-gl-ffi-PATH=git://github.com/sthilaid/open-gl-ffi.git
# gl-fonts-PATH=git://github.com/sthilaid/gl-fonts.git
# sdl-interface-PATH=git://github.com/sthilaid/sdl-interface.git
# export state-machine-PATH=git://github.com/sthilaid/state-machine.git

export class-PATH=/home/dave/projet/maitrise/class
export thread-simulation-PATH=/home/dave/projet/maitrise/thread-simulation
export scm-lib-PATH=/home/dave/projet/maitrise/scm-lib
export open-gl-ffi-PATH=/home/dave/projet/scheme/open-gl-ffi
export gl-fonts-PATH=/home/dave/projet/maitrise/gl-fonts
export sdl-interface-PATH=/home/dave/projet/maitrise/sdl-interface
export state-machine-PATH=/home/dave/projet/maitrise/state-machine

##############################################################################
## Comilation flags
##############################################################################
LD_OPTIONS_LIN = -lutil -lSDL -lSDL_mixer -lglut
LD_OPTIONS_COMMON =-L$(GAMBIT_LIB) -L$(GL_LIB) -L$(SDL_LIB) -L$(SDL_mixer_LIB) -lgambc 

PATH_TO_GL=/usr
GL_INCLUDE=$(PATH_TO_GL)/include/GL
GL_LIB=$(PATH_TO_GL)/lib

PATH_TO_SDL=/usr
SDL_INCLUDE=$(PATH_TO_SDL)/include/SDL
SDL_LIB=$(PATH_TO_SDL)/lib

PATH_TO_SDL_mixer=/usr
SDL_mixer_INCLUDE=$(PATH_TO_SDL_mixer)/include/SDL
SDL_mixer_LIB=$(PATH_TO_SDL_mixer)/lib

ALL_SDL_INCLUDE=-I$(SDL_INCLUDE) -I$(SDL_mixer_INCLUDE)

INCLUDE_OPTIONS=-I$(GAMBIT_INCLUDE) -I$(GL_INCLUDE) $(ALL_SDL_INCLUDE)
LD_OPTIONS = $(LD_OPTIONS_COMMON) $(LD_OPTIONS_LIN)


.SUFFIXES:
.SUFFIXES: .c .scm .o .o1 .m
.PHONY: all clean shared-objects tarball welcome

##############################################################################
## Compilation Targets
##############################################################################

all: welcome prefix include lib

prefix:
ifneq "$(PREFIX)" "."
	mkdir -p $(PREFIX)
endif

include: $(foreach f,$(INCLUDE_FILES),$(INCLUDE_PATH)/$(f))
$(INCLUDE_PATH)/%.scm: $(SRC_PATH)/%.scm
	mkdir -p $(INCLUDE_PATH)
	cp $< $@

lib: $(foreach f,$(GAME_FILES),$(LIB_PATH)/$(f))
$(LIB_PATH)/%.o1: $(SRC_PATH)/%.scm
	mkdir -p $(LIB_PATH)
	$(GSC) -cc-options "$(INCLUDE_OPTIONS)" -ld-options "$(LD_OPTIONS)" -o $@ $<

stringify = $(foreach f,$(1),"$(f)")
devel: $(SRC_PATH)/game-loader.scm \
	     $(addprefix $(SRC_PATH)/,$(LIB_FILES:.o1=.scm)) \
       $(addprefix $(LIB_PATH)/,$(COMPILED_FILES))
	$(GSI) $< -e '(load-game $(call stringify,$(SRC_PATH)) $(call stringify,$(LIB_PATH)) (list $(call stringify,$(GAME_FILES))))'

run-game: $(addprefix $(LIB_PATH)/,$(GAME_FILES))
	@echo "*** Compilation Finished ***"
	@echo
	@echo "Launching game...."
	@echo
	$(GSI) $^ -e '(main)'

$(LIB_PATH)/font-%.o1: generated/font-%.scm
	mkdir -p $(LIB_PATH)
	$(GSC) -cc-options "$(INCLUDE_OPTIONS)" -ld-options "$(LD_OPTIONS)" -o $@ $<

generated/font-%.scm: fonts/%.ppm $(SRC_PATH)/user-interface-images.scm
	@echo Generating font scm file $@
	mkdir -p generated
	gsi $(SRC_PATH)/user-interface-images.scm -e "(generate-font-file \"$(<)\")"

static: $(addprefix $(SRC_PATH)/,$(LIB_FILES:.o1=.scm)) 
	$(GSC) -exe -o $(PREFIX)/lode-runner -cc-options "$(INCLUDE_OPTIONS)" -ld-options "$(LD_OPTIONS)" -prelude '(define-cond-expand-feature compiled-version)' $^

### "included" macro dependant scheme source files

# lousy dep: all lib files depend on the header files...
$(addprefix $(LIB_PATH)/,$(LIB_FILES)): include

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

##############################################################################
### External Scheme library dependencies
##############################################################################
$(eval $(call define-dependency,scm-lib,scm-lib.scm scm-lib_.scm))
$(eval $(call define-dependency,open-gl-ffi,opengl.scm opengl_.scm \
                                glu.scm glu_.scm))
$(eval $(call define-dependency,sdl-interface,sdl-interface.scm))
$(eval $(call define-dependency,gl-fonts,ppm-reader.scm texture.scm \
                                texture_.scm sprite.scm sprite_.scm \
	                              font.scm font_.scm))
$(eval $(call define-dependency,class,class.scm class_.scm))
$(eval $(call define-dependency,thread-simulation, rbtree.scm match.scm \
                                  thread-simulation.scm \
                                  thread-simulation_.scm ))
#$(eval $(call define-dependency,state-machine,state-machine.scm))

clean:
	rm -rf generated $(INCLUDE_PATH) $(LIB_PATH) $(EXTERNAL_LIBS) $(SRC_PATH)/*.[oc] $(PREFIX)/lode-runner
