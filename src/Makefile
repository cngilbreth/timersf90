include $(HOME)/Projects/make.inc

# Set afmc version string here -- it will be compiled in
AFMC_VER=1.16.0b

FC := $(MPIFC) -DTEST_TIMERS

################################################################################
# Targets
################################################################################

vpath %.o build
vpath %.mod mod
vpath %.f90 ./

OBJ1=timers.o

timers: ${OBJ1}
	${FC} -o $@ $+

clean:
	/bin/rm -f *.o *.mod camc build/*.o mod/*.mod version.h ../camc

################################################################################
# Object files
################################################################################

build/timers.o:

################################################################################
# Build rules
################################################################################

build/%.o: %.cpp
	$(CXX) -c $< -o $@
build/%.o: %.c
	$(CC) -c $< -o $@
build/%.o: %.f90
	$(FC) -c $< -o $@
build/%.o: %.f
	$(FC) -c $< -o $@
build/%.o: %.F90
	$(FC) -c $< -o $@
build/%.o: %.F
	$(FC) -c $< -o $@
