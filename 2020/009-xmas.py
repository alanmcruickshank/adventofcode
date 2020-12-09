"""Advent of Code Day 9

https://adventofcode.com/2020/day/9
"""

import itertools

inputs = [
    ("009-xmas-test.txt", 5),
    ("009-xmas-input.txt", 25)
]


def find_invalid(stream, preamble_length=25):
    # Start after the preamble
    idx = preamble_length
    while idx < len(stream):
        # Slice
        reference = stream[idx - preamble_length: idx]
        for a, b in itertools.combinations(reference, 2):
            if a + b == stream[idx]:
                break
        else:
            return idx, stream[idx]
        idx += 1
    return None
    

for in_file, preamble_length in inputs:
    print(in_file)
    # Read the stream
    with open(in_file) as txt_file:
        stream = [int(val) for val in txt_file.readlines()]
    
    idx, invalid = find_invalid(stream, preamble_length)
    print(idx, invalid)

    # Search for weakness.
    found = False
    weakness_stream = None
    for idx_start in range(idx):
        for idx_end in range(idx_start, idx):
            weakness_stream = stream[idx_start: idx_end]
            total = sum(weakness_stream)
            if total == invalid:
                found = True
                break
            elif total > invalid:
                break
        if found:
            break
    else:
        ValueError("NOT FOUND!?")
    
    weakness_key = (min(weakness_stream), max(weakness_stream))
    print(weakness_key, sum(weakness_key))

