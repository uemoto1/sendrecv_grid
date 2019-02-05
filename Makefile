TARGET = sendrecv_grid

FC = mpif90
FLAGS = -cpp -fopenmp -Wall  -DSALMON_USE_MPI 

# FC = mpiifort
# FLAGS =  -fpp -xMIC-AVX512 -qopenmp  -warn all  -DSALMON_USE_MPI

OBJS = \
misc/unusedvar.o \
parallel/salmon_communication.o \
parallel/salmon_parallel.o \
test00.o \
main.o


.SUFFIXES: .f90

%.o: %.f90
	$(FC) -c -o $@ $^ $(FLAGS)

$(TARGET): $(OBJS)
	$(FC) -o $@ $^ $(FLAGS)

clean:
	rm $(TARGET) $(OBJS)
