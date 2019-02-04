
TARGET = test
OBJS = \
misc/unusedvar.o \
parallel/salmon_communication.o \
parallel/salmon_parallel.o \
test.o

FFLAGS = -O3 -cpp -fopenmp -Wall  -DSALMON_USE_MPI 

FC = mpif90

.SUFFIXES: .f90

%.o: %.f90
	$(FC) -c -o $@ $^ $(FFLAGS)

%.mod: %.f90 %.o
	@:

$(TARGET): $(OBJS)
	$(FC) -o $@ $^ $(FFLAGS) $(LIBS)

# item.o: using_module.mod


clean:
	rm $(TARGET) $(OBJS)
