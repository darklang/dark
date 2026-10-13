import json, harness

def work(i):
    item = {"id": i, "name": "item", "tags": ["a", "b", "c"], "active": True}
    s = json.dumps(item, separators=(",", ":"))
    back = json.loads(s)
    return len(s) + back["id"]

def main():
    acc = 0
    for i in range(1, 2001):
        acc += work(i)
    return acc

harness.run(main)
