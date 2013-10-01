# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%  $Id: Makefile,v 1.0 2013/05/22 hcp Exp $                         %%       
# %%  !MODULE: Makefile                                                %%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Define variables
EXE     := readnc
NETCDF  := /home/chenchuchu/netcdf
ROOTDIR := ..
BIN     := $(ROOTDIR)/bin
HDR     := $(ROOTDIR)/Headers
MOD     := $(ROOTDIR)/mod
LIB     := $(ROOTDIR)/lib
UTIL    := $(ROOTDIR)/GeosUtil

#========================================================================
# List of files to compile
#========================================================================

# List of source files
SRC := $(wildcard *.f) $(wildcard *.F) $(wildcard *.f90) $(wildcard *.F90)

# Replace .f, .F, .f90 and .F90 extensions with *.o
TMP := $(SRC:.f=.o)
TMQ := $(TMP:.F=.o)
TMR := $(TMQ:.f90=.o)
OBJ := $(TMR:.F90=.o)

#========================================================================
# IFORT compilation options
#========================================================================
FC       = ifort
FFLAGS   = -cpp -w -O0 -auto -noalign -convert big_endian -g -openmp \
           -Dmultitask -gstabs+

# Include options (i.e. for finding *.h, *.mod files)
INCLUDE  = -I$(HDR) -module $(MOD) -I$(NETCDF)/include

# Link to library files created from code in the various subdirs
LINK     = -L$(LIB) -lGeosUtil -L$(NETCDF)/lib -lnetcdf

F90      = $(FC) $(FFLAGS) $(INCLUDE)
LD       = $(FC) $(FFLAGS)
FREEFORM = -free

#========================================================================
# Makefile targets
#========================================================================
.PHONY: clean realclean

all:
	@$(MAKE) lib
	@$(MAKE) exe

lib: libutil libcore

libcore: $(OBJ)

libutil:
	@$(MAKE) -C $(UTIL)

exe:
	$(LD) $(OBJ) $(LINK) -o $(EXE)
	cp -f $(EXE) $(BIN)

clean:
	rm -f *.o *.mod $(EXE)

realclean:
	@$(MAKE) clean
	@$(MAKE) -C $(UTIL) clean
	rm -f $(LIB)/*.a
	rm -f $(MOD)/*.mod
	rm -f $(BIN)/$(EXE)


#========================================================================
# Specify pattern rules for compiliation
# (i.e. tell "make" how to compile different types of source code files)
#========================================================================
%.o : %.f
	$(F90) -c $<
%.o : %.F
	$(F90) -c $<
%.o : %.f90
	$(F90) -c $(FREEFORM) $<
%.o : %.F90
	$(F90) -c $(FREEFORM) $<

#========================================================================
# Dependencies listing (grep "USE " to get the list of module references!)
#
# From this list of dependencies, the "make" utility will figure out the
# correct order of compilation (so we don't have to do that ourselves).
#========================================================================

diag3.o                     : diag3.f			      diag_mod.o           \
							  logical_mod.o

cleanup.o                   : cleanup.f		          diag_mod.o

dao_mod.o	                : dao_mod.f		          logical_mod.o

diag49_mod.o                : diag49_mod.f            logical_mod.o

emissdr.o                   : emissdr.f               logical_mod.o        \
                              dao_mod.o               diag_mod.o           \
							  soil_nh3_mod.o       

emissions_mod.o             : emissions_mod.f                              

main.o			            : main.f		          input_mod.o	       \
                              dao_mod.o	              logical_mod.o	       \
							  rdmcip_mod.o			  emissions_mod.o      \
							  diag49_mod.o            wrsnh3_mod.o         \
							  soilnh3_restart_mod.o

rdmcip_mod.o                : rdmcip_mod.f90	dao_mod.o	netcdf_mod.o

rdlai.o                     : rdlai.f

readlai.o                   : readlai.f               logical_mod.o

rdland.o                    : rdland.f                logical_mod.o

rdsoil.o                    : rdsoil.f

wrsnh3_mod.o                : wrsnh3_mod.f90          diag_mod.o            \
							  netcdf_mod.o