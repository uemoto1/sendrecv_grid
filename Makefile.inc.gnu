TARGET = sendrecv_grid

FC = mpif90
FLAGS = -cpp -fopenmp -Wall  -DSALMON_USE_MPI  -DSALMON_DEBUG_SENDRECV_GRID


