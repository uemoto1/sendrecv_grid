#!/usr/bin/env python3
import numpy as np

result = []
for n in [16, 32, 48, 64, 96]:
    row = [m]
    for m in [3, 4, 5]:
        name = "%02d_%02d" % (m, n)
        for i in range(m*m*m):
            tmp = []
            with open("test004/%s/log%06d.txt" % (name, i)) as fh:
                for line in fh:
                    if "cputime" in line:
                        tmp.append( float(line.split(":")[-1]) )
        row += [np.average(tmp)]
    result += [row]

np.savetxt("test004_result2.txt", result)


