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

def populate_path_counts(devices, start=0, end_diff=3, max_diff=3):
    path_counts = {}
    devices = devices.copy()
    # Add the end point
    devices.append(max(devices) + end_diff)
    # Add the start point
    devices.append(start)
    # Sort first. This is important.
    devices = sorted(devices)
    # Add the start point
    path_counts[start] = 1
    # Iterate through devices, adding up the paths
    for idx, dev in enumerate(devices):
        # Where can I get to from this point?
        # We know it will be within the next three points
        for next_dev in devices[idx + 1 : idx + 1 + max_diff]:
            if next_dev - dev > max_diff:
                continue
            if next_dev in path_counts:
                path_counts[next_dev] += path_counts[dev]
            else:
                path_counts[next_dev] = path_counts[dev]
    return path_counts

for fname in inputs:
    print(fname)
    devs = load_and_sort(fname)
    diff_iter = iter_diffs(devs)
    counts = count_diffs(diff_iter)
    print(counts)
    print(counts[1] * counts[3])
    # Part 1 answer: 1984

    # Part 2 is graph theory.
    # Dijkstras algorithm? Not quite.
    # For each adapter, we need to know how many ways we can get TO IT.

    device_rating = max(devs) + 3
    # We cannot get to a device from another one higher than it.
    # Therefore work through the adapters, populating
    # a set of ways you could arrive at this point.
    # We need to add up the ways as we go.
    path_counts = populate_path_counts(devs)
    print(path_counts[device_rating])
    # Part 2 answer: 3543369523456
