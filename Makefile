FC=ifort
PFUNIT = /opt/funit/pfunit-serial
F90_VENDOR = Intel

SDIR=src
ODIR=out

PATHS = PATH='/opt/intel/compilers_and_libraries_2018.2.199/linux/mpi/intel64/bin:'"$$PATH" LD_LIBRARY_PATH='/opt/intel/compilers_and_libraries_2018.2.199/linux/compiler/lib/intel64_lin' 
FC:=$(PATHS) $(FC)
FFLAGS = -funroll-all-loops -std08 -implicitnone -fpp -warn all -pedantic -module $(ODIR) -coarray
FFLAGS += -I$(PFUNIT)/mod -I$(PFUNIT)/include 
FFLAGS += -WB -g -O0
LIBS = $(PFUNIT)/lib/libpfunit$(LIB_EXT)

FS = $(filter-out $(SDIR)/main.F90, $(wildcard $(SDIR)/*.F90))
OBJS = $(FS:$(SDIR)/%.F90=$(ODIR)/%.o)

.PHONY: all main run

all: $(ODIR)/main.o

run: $(ODIR)/main.o
	$(PATHS) $(ODIR)/main.o 10 1 1

main: $(ODIR)/main.o

$(ODIR)/main.o:  $(SDIR)/main.F90 
	$(PATHS) $(FC) $^ -o $@ $(FFLAGS)

$(ODIR)/%.o: $(SDIR)/%.F90
	$(PATHS) $(FC) -c $< -o $@ $(FFLAGS)
