#!/usr/bin/env python3
import numpy as np

result = []
for m in [2, 3, 4, 5, 6, 7]:
    row = [m]
    for n in [32, 48, 64]:
        name = "%02d_%02d" % (m, n)
        for i in range(m*m*m):
            tmp = []
            with open("test004/%s/log%06d.txt" % (name, i)) as fh:
                for line in fh:
                    if "cputime" in line:
                        tmp.append( float(line.split(":")[-1]) )
        row += [
            np.amin(tmp), np.average(tmp), np.max(tmp)
        ]
    result.append(row)

np.savetxt("test004_result.txt", result)


