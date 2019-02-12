#!/usr/bin/env python3
import numpy as np
import sys

# mt, mx, my, mz, nx, ny, nz, nb, nd = map(int, sys.argv[1:])
mt, mx, my, mz, nx, ny, nz, nb, nd = 2, 4, 1, 1, 12, 14, 16, 10, 4

output = "test002_result.npz"

ox, oy, oz = nx + 2 * nd, ny + 2 * nd, nz + 2 * nd
buf = np.empty([mt, mx, my, mz, ox, oy, oz, nb], dtype=float)
for i, (ix, iy, iz) in enumerate(np.ndindex(mx, my, mz)):
    print("# Loading %d..." % i)
    jx, jy, jz, jb, tmp = np.loadtxt("log%06d.txt" % i, dtype="int,int,int,int,float", unpack=True)
    buf[:, ix, iy, iz, :, :, :, :] = tmp.reshape([mt, ox, oy, oz, nb])

print("# Writing %s" % output)
np.savez_compressed(
    output, 
    result=buf,
    x=np.arange(1-nd, nx+nd+1),
    y=np.arange(1-nd, ny+nd+1),
    z=np.arange(1-nd, nz+nd+1),
)
