#!/usr/bin/env python3
import numpy as np
import sys

# mt, mx, my, mz, nx, ny, nz, nb, nd = map(int, sys.argv[1:])
mt, mx, my, mz, nx, ny, nz, nb, nd = 2, 4, 1, 1, 12, 14, 16, 10, 4

buf = np.empty([mt, mx, my, mz, nx+2*nd, ny+2*nd, nz+2*nd, nb], dtype=float)

for i, (it, ix, iy, iz) in enumerate(np.ndindex(mt, mx, my, mz)):
    print("# Loading %d..." % i)
    jx, jy, jz, jb, tmp = np.loadtxt("log%06d.txt" % i, dtype="int,int,int,int,float", unpack=True)
    buf[it, ix, iy, iz, :, :, :, :] = tmp.reshape([nx+2*nd, ny+2*nd, nz+2*nd, nb])

np.savez_compressed("test002_result.npz", buf)

