OOPS_FILES = format.scm mechanics.scm slots.scm instance.scm class.scm subclass.scm setget.scm class-hierarchy.scm define-class.scm methods.scm generics.scm describe.scm limited-types.scm collection-classes.scm conditions.scm collection-methods.scm ts-table.scm


## compilers
GSC=$(PATH_TO_GAMBIT)/bin/gsc -:=$(PATH_TO_GAMBIT) -debug
CC=gcc

## Gambit-c
PATH_TO_GAMBIT=/usr/local/Gambit-C/current
GAMBIT_LIB=$(PATH_TO_GAMBIT)/lib
GAMBIT_INCLUDE=$(PATH_TO_GAMBIT)/include

INCLUDE_OPTIONS=-I$(GAMBIT_INCLUDE)
LD_OPTIONS =-L$(GAMBIT_LIB) -lgambc


.SUFFIXES:
.SUFFIXES: .c .scm .o .so
.PHONY: all clean 


all: liboops.so


liboops.so: $(OOPS_FILES:.scm=.o) liboops_.o
	$(CC) -shared -o $@ $^ $(LD_OPTIONS)

liboops_.c: $(OOPS_FILES:.scm=.c)
	$(GSC) -link -o $@ $^

.c.o:
	$(CC) $(INCLUDE_OPTIONS) -c $*.c

.scm.c: 
	$(GSC) -c -e '(load "oops.scm")' $* 

ALL_SCM = $(wildcard *.scm)
clean:
	rm -f $(ALL_SCM:.scm=.c) *_.c *.o* liboops.so *.tar.gz *.tgz *.~*~ *.zip
