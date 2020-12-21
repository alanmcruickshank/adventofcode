"""Advent of Code Day 20

https://adventofcode.com/2020/day/20

Image borders.

Binary?
"""

from collections import defaultdict
from itertools import permutations


class Tile:
    def __init__(self, raw):
        self.raw = raw.strip()
        self.rows = []
        for line in self.raw.split('\n'):
            if line.startswith("Tile "):
                self.tile_no = int(line.partition(":")[0][5:])
            else:
                self.rows.append(line.strip())
        
        # NB: Clockwise.
        sides = [
            self.rows[0],  # Top rightwards
            ''.join(row[-1] for row in self.rows),  # Right downwards
            # NB: Bottom and left are still forward so matching works
            self.rows[-1],  # Bottom
            ''.join(row[0] for row in self.rows),  # Left
        ]

        # Flip the shape horizontally
        flip_h_sides = [
            sides[0][::-1], # Top backwards
            sides[3],
            sides[2][::-1], # Bottom backwards
            sides[1],
        ]

        # Flip the shape vertically
        flip_v_sides = [
            sides[2],
            sides[1][::-1],
            sides[0],
            sides[3][::-1],
        ]

        # Flip the shape vertically and horizontally
        flip_hv_sides = [
            sides[2][::-1],
            sides[3][::-1],
            sides[0][::-1],
            sides[1][::-1],
        ]

        self.keys = []
        
        for side_keys, hflip, vflip in [
            (sides, False, False),
            (flip_h_sides, True, False),
            (flip_v_sides, False, True),
            (flip_hv_sides, True, True),
        ]:
            self.keys += [
                {'key': self.tilestring_to_int(s), 'tile': self, 'side': idx, 'hflip': hflip, 'vflip': vflip}
                for idx, s in enumerate(side_keys)
            ]
        
        # A deduped set for searching?
        # self.key_set = set(
        #     key['key'] for key in self.keys
        # )

    @staticmethod
    def tilestring_to_int(s):
        return int(s.replace('#', '1').replace('.', '0'), 2)
    
    def __repr__(self):
        return "<Tile {0}: {1}>".format(self.tile_no, self.rows[0][:5])


class TileSet:
    def __init__(self, fname):
        with open(fname) as f:
            raw_tiles = f.read().split("\n\n")
        self.tiles = {}
        for raw_tile in raw_tiles:
            tile = Tile(raw_tile)
            self.tiles[tile.tile_no] = tile
    
    def find_links(self):
        keys = defaultdict(list)
        for tile_no in self.tiles:
            for key in self.tiles[tile_no].keys:
                keys[key['key']].append(key)
        return keys
    
    def pruned_links(self):
        links = self.find_links()
        pruned_links = {}
        for key in links:
            distinct_linked_tiles = len(set(k['tile'].tile_no for k in links[key]))
            if distinct_linked_tiles <= 1:
                # No matches. Prune.
                continue
            if distinct_linked_tiles > 2:
                # Conflicting match. Skip this for now.
                print("Skipping potentially conflicted match...[{0}]".format(key))
                continue
            pruned_links[key] = links[key]
        return pruned_links
    
    def assemble(self):
        pruned_links = self.pruned_links()
        # Doesn't really matter where we start.
        layout = {}
        available_tiles = list(self.tiles.keys())
        # Put the first tile at the origin.
        assembled_tiles = [available_tiles.pop(0)]
        # Tile neighbors
        neighbors = defaultdict(set)
        for key in pruned_links:
            distinct_tiles = set(k['tile'].tile_no for k in pruned_links[key])
            for a, b in permutations(distinct_tiles, 2):
                neighbors[a].add(b)
        return neighbors

    def find_corner_product(self):
        neighbors = self.assemble()
        corners = []
        for key in neighbors:
            if len(neighbors[key]) == 2:
                corners.append(key)
        return self.product(corners)
    
    @staticmethod
    def product(iterable):
        prod = 1
        for elem in iterable:
            prod *= elem
        return prod

for fname in ["020-tiles-1.txt", "020-tiles-2.txt"]:
    print(fname)
    ts = TileSet(fname)
    print(ts.find_corner_product())

