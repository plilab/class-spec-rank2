import os

os.chdir(
    "dist-newstyle/build/x86_64-linux/ghc-9.8.4/optimizing-syb-0.1.0.0/b/suite/build/suite/suite-tmp/benchmarks"
)

dirs = ["Hand", "SYB", "SYB35", "SYB35Opt", "SYBOpt", "SYBPEOnly"]
total_time = []
num_files = []
for dir in dirs:
    os.chdir(dir)
    files = os.listdir()
    files = list(filter(lambda x: x[-12:] == "dump-timings", files))
    temp_tot = 0
    sz = 0
    for o in files:
        file = list(filter(bool, open(o).read().split("\n")))
        timed = list(map(lambda x: float(x.split("=")[-1]), file))
        print(f"{dir}/{o}: {sum(timed)}")
        temp_tot += sum(timed)
        sz += 1
    total_time.append(temp_tot)
    num_files.append(sz)
    os.chdir("..")

for k, v, e in zip(dirs, total_time, num_files):
    print(f"{k}:{v}, avg: {v / e}")
