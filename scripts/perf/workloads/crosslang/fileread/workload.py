import harness

def main():
    with open("rundir/crosslang-lines.txt") as f:
        lines = f.read().split("\n")
    a = sum(1 for l in lines if "LEVEL=" in l)
    b = sum(1 for l in lines if "ERROR" in l)
    return a + b

harness.run(main)
