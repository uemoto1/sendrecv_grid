include Makefile.inc

TARGET = sendrecv_test

OBJS = \
misc/unusedvar.o \
parallel/salmon_communication.o \
parallel/salmon_parallel.o \
common/structures.o \
common/pack_unpack.o \
common/sendrecv_grid.o \
test00.o \
main.o


.SUFFIXES: .f90

%.o: %.f90
	$(FC) -c -o $@ $^ $(FLAGS)

$(TARGET): $(OBJS)
	$(FC) -o $@ $^ $(FLAGS)

clean:
	rm $(TARGET) $(OBJS) *.mod
