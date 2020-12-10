"""Advent of Code Day 10

https://adventofcode.com/2020/day/10
"""


inputs = [
    "010-jolts-1.txt",
    "010-jolts-2.txt",
    "010-jolts-3.txt",
]

def load_and_sort(fname):
    with open(fname) as f:
        return sorted([int(j) for j in f.readlines()])

def iter_diffs(devices, start=0, end_diff=3):
    # Copy the device list
    devices = devices.copy()
    current_jolts = start
    while devices:
        next_dev = devices.pop(0)
        if next_dev > current_jolts + 3:
            raise ValueError("Jump too big! {0} -> {1}".format(current_jolts, next_dev))
        yield next_dev - current_jolts
        current_jolts = next_dev
    # Yield the built in diff too
    yield end_diff

def count_diffs(diff_iter):
    counts = {}
    for d in diff_iter:
        if d not in counts:
            counts[d] = 0
        counts[d] += 1
    return counts

for fname in inputs:
    print(fname)
    devs = load_and_sort(fname)
    diff_iter = iter_diffs(devs)
    counts = count_diffs(diff_iter)
    print(counts)
    print(counts[1] * counts[3])
    # Part 1 answer: 1984
