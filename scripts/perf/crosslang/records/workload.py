import harness

class Point:
    __slots__ = ("x", "y", "label")
    def __init__(self, x, y, label):
        self.x = x
        self.y = y
        self.label = label

def step(p):
    return Point(p.x + 1, p.y + 2, p.label)

def work(i):
    p = Point(i, i + 1, "p")
    q = step(step(p))
    return q.x + q.y

def main():
    acc = 0
    for i in range(1, 20001):
        acc += work(i)
    return acc

harness.run(main)
