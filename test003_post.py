#!/usr/bin/env python3
import numpy as np
import sys

# mx, my, mz, nx, ny, nz, nb = map(int, sys.argv[1:])
mx, my, mz, nx, ny, nz, nb = 2, 2, 4, 12, 14, 16, 2

buf = np.empty([mx, my, mz, nx, ny, nz, nb], dtype=float)

for i, (ix, iy, iz) in enumerate(np.ndindex(mx, my, mz)):
    print("# Loading %d..." % i)
    jx, jy, jz, jb, tmp = np.loadtxt("log%06d.txt" % i, dtype="int,int,int,int,float", unpack=True)
    buf[ix, iy, iz, :, :, :, :] = tmp.reshape([nx, ny, nz, nb])

print("# Writing test003_result.npz")
np.savez_compressed("test003_result.npz", buf)

