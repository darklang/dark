import harness

def main():
    acc = 0
    for i in range(1, 300001):
        acc = (acc + i * i) % 1000003
    return acc

harness.run(main)
