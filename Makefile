.SUFFIXES: .f .F .F90 .f90 .o .mod
.SHELL: /bin/sh

.PHONY : usage
usage:
	@echo ""
	@echo "    * USAGE * "
	@echo ""
	@echo " make test       : compiles the test program test_vecode.x"
	@echo " make clean      : cleans object and executable files"
	@echo ""


objdir = .obj

ifort ?= 0
debug ?= 0 

ifeq ($(ifort),1)
    FC = ifort 
    LIB = /home/robinson/apps/netcdf/netcdf/lib
    INC = /home/robinson/apps/netcdf/netcdf/include
else
    FC = gfortran
    #LIB = /usr/lib
    #INC = /usr/include
    LIB = /opt/local/lib
    INC = /opt/local/include
endif 

ifeq ($(ifort),1)
	## IFORT OPTIONS ##
	FLAGS        = -module $(objdir) -L$(objdir) -I$(INC)
	LFLAGS		 = -L$(LIB) -lnetcdf

	ifeq ($(debug), 1)
	    DFLAGS   = -C -traceback -ftrapuv -fpe0 -check all -vec-report0
	    # -w 
	else
	    DFLAGS   = -vec-report0 -O3
	endif
else
	## GFORTRAN OPTIONS ##
	FLAGS        = -I$(objdir) -J$(objdir) -I$(INC)
	LFLAGS		 = -L$(LIB) -lnetcdff -lnetcdf

	ifeq ($(debug), 1)
	    DFLAGS   = -w -p -ggdb -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace -fcheck=all
	else
	    DFLAGS   = -O3
	endif
endif

## Individual libraries or modules ##
$(objdir)/ncio.o: ncio.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

$(objdir)/vecode.o: vecode.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

$(objdir)/vecode_physics.o: vecode_physics.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

$(objdir)/tvm.o: tvm.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

## Complete programs

vecode: $(objdir)/ncio.o $(objdir)/vecode.o
	$(FC) $(DFLAGS) $(FLAGS) -o test_vecode.x $^ test_vecode.f90 $(LFLAGS)
	@echo " "
	@echo "    test_vecode.x is ready."
	@echo " "

clean:
	rm -f test_vecode.x $(objdir)/*.o $(objdir)/*.mod

