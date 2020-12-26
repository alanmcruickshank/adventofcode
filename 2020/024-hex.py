"""Advent of Code Day 24

https://adventofcode.com/2020/day/24

Hexagons and Parsing.

Hexagonal grid is just like a slanted square one.
    positive real is E
    positive complex NW
Parsing is just iterative.
"""

def parse_row(s):
    while s:
        if s[0] in "ns":
            yield s[:2]
            s = s[2:]
        else:
            yield s[0]
            s = s[1:]


def convert(s):
    return {
        "e": complex(1),
        "w": complex(-1),
        "nw": complex(0, 1),
        "ne": complex(1, 1),
        "sw": complex(-1, -1),
        "se": complex(0, -1)
    }[s.lower()]


class Grid:
    def __init__(self):
        self._flipped = set()
    
    def flip(self, coord):
        if coord in self._flipped:
            self._flipped.remove(coord)
        else:
            self._flipped.add(coord)
    
    def size(self):
        return len(self._flipped)


for fname in ['024-1.txt', '024-2.txt']:
    print("File:", fname)
    grid = Grid()
    with open(fname) as f:
        for row in f.readlines():
            coord = sum([convert(e) for e in parse_row(row.strip())])
            grid.flip(coord)
    print(grid.size())
    # Part 1 answer: 438



