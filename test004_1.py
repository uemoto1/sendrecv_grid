#!/usr/bin/env python3
import numpy as np
import sys

nd = 4
mx, my, mz, nx, ny, nz, nb = map(int, sys.argv[2:])

ox, oy, oz = nx + 2 * nd, ny + 2 * nd, nz + 2 * nd

buf = np.empty([mx, my, mz, ox, oy, oz, nb], dtype=float)

for i, (ix, iy, iz) in enumerate(np.ndindex(mx, my, mz)):
    print("# Loading %d..." % i)
    jx, jy, jz, jb, tmp = np.loadtxt("log%06d.txt" % i, dtype="int,int,int,int,float", unpack=True)
    buf[ix, iy, iz, :, :, :, :] = tmp.reshape([ox, oy, oz, nb])

print("# Writing.. ")
np.savez_compressed(
    sys.argv[1], 
    result=buf,
    x=np.arange(1-nd, nx+nd+1),
    y=np.arange(1-nd, ny+nd+1),
    z=np.arange(1-nd, nz+nd+1),
)

