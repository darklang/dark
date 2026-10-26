import harness

def work(i):
    base = "field-" + str(i)
    parts = [base + ":" + str(j) for j in range(1, 9)]
    joined = ",".join(parts)
    back = joined.split(",")
    return len(joined) + len(back)

def main():
    acc = 0
    for i in range(1, 5001):
        acc += work(i)
    return acc

harness.run(main)
