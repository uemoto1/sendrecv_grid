#!/usr/bin/env python3
import os

with open("job.txt") as fh:
    job_template = fh.read()

if not os.path.exists("test004"):
    os.mkdir("test004")

result = ["cd test004"]

for m in [2, 3, 4, 5, 6, 7, 8]:
    nproc = m ** 3
    for n in [32, 48, 64]:
        name = "%02d_%02d" % (m, n)
        with open("test004/%s.sh" % name, "w") as fh:
            fh.write(job_template.format(
                NAME=name,
                NPROC=nproc,
                MX=m, MY=m, MZ=m,
                NX=n, NY=n, NZ=n,
                NB=8,
            ))
        result.append("pjsub %s.sh" % name)
print("\n".join(result))
