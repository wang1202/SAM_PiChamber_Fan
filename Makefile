# Makefile for various platforms
# Execute using Build csh-script only!
# Used together with Perl scripts in SRC/SCRIPT 
# (C) 2005 Marat Khairoutdinov
# $Id: Makefile 1595 2014-10-30 21:36:30Z dschanen@uwm.edu $
#------------------------------------------------------------------
# uncomment to disable timers:
#
#NOTIMERS=-DDISABLE_TIMERS
#-----------------------------------------------------------------

SAM = SAM_$(ADV_DIR)_$(SGS_DIR)_$(RAD_DIR)_$(MICRO_DIR)_32

# Determine platform 
PLATFORM := $(shell uname -s)

#------------------------------------------------------------------------
# This Makefile is for SAM running on Cori Intel at NERSC
#
ifeq ($(PLATFORM),Linux)
# Cori Intel 
CC          = cc -c -DLINUX
FF77        = ftn -c
FF90        = ftn -c 
LD          = ftn

INC_NETCDF := $(NETCDF_DIR)/include
LIB_NETCDF := $(NETCDF_DIR)/lib
INC_MPI    := $(MPICH_DIR)/include
LIB_MPI    := $(MPICH_DIR)/lib

##intel compiler
#FFLAGS = -debug full -CB -g  -extend-source 132 -init=snan,arrays -traceback -check bounds -check uninit -ftrapuv
FFLAGS = -O3 -extend-source 132
FFLAGS     += -r8 
FFLAGS     += -I${INC_MPI} -I${INC_NETCDF} -DNETCDF

##CFFLAGS     = -I${INC_MPI} -I${INC_NETCDF} -DNETCDF
LDFLAGS     = -L${LIB_MPI} -L${LIB_NETCDF} -lnetcdf -lnetcdff
##LDFLAGS    += -mkl

endif

#----------------------------------
#----------------------------------------------
# you don't need to edit below this line


#compute the search path
dirs := . $(shell cat Filepath)
VPATH    := $(foreach dir,$(dirs),$(wildcard $(dir))) 

.SUFFIXES:
.SUFFIXES: .f .f90 .F90 .F .c .o



all: $(SAM_DIR)/$(SAM)

SOURCES   := $(shell cat Srcfiles)

Depends: Srcfiles Filepath
	$(SAM_SRC)/SCRIPT/mkDepends Filepath Srcfiles > $@

Srcfiles: Filepath
	$(SAM_SRC)/SCRIPT/mkSrcfiles > $@

OBJS      := $(addsuffix .o, $(basename $(SOURCES))) 

$(SAM_DIR)/$(SAM): $(OBJS)
	$(LD) -o $@ $(OBJS) $(LDFLAGS)

.F90.o:
	${FF90}  ${FFLAGS} $(if $(filter $@, $(WARNABLE) ), $(WARNINGS) ) $<;
.f90.o:
	${FF90}  ${FFLAGS} $(if $(filter $@, $(WARNABLE) ), $(WARNINGS) ) $<;
.f.o:
	${FF77}  ${FFLAGS} $<
.F.o:
	${FF77}  ${FFLAGS} $<
.c.o:
	${CC}  ${CFLAGS} -I$(SAM_SRC)/TIMING $(NOTIMERS) $<

include Depends
