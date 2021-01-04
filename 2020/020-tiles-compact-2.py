"""Advent of Code Day 20

https://adventofcode.com/2020/day/20

Image borders.

A more compact implementation.
Could use bitarray to do this in a more
memory efficient way, but the equivalent
method can be done using lists.

No 2d arrays, just a long 1d array, but
with clever slicing to make it look seamless
under the hood and deal with comparisons and
matching. Where we do matching, they should
be done WITHOUT type conversion and to
compare the lists directly.

For this implementation, we'll use lists
of integers. We'll need to implement functions
to do element-wise comparisons and operations.
This could be done using a class, but I expect
functions will be a more concise implementation
here.

Coordinates as before are done using complex
values.
"""

from collections import defaultdict
from itertools import permutations, product

# Use enumerate to get the index if needed.
DIRECTIONS = [complex(0, 1)**i for i in range(4)]


class Tile:
    def __init__(self, vals, width):
        """2D Array of integers.

        Implemented as clever indexing on a 1d array.
        Height is calculated from vals length and width.
        """
        self._vals = vals
        self.length = len(self._vals)
        self.width = width
        if self.length % self.width != 0:
            raise ValueError("Length of vals is not an integer multiple length of width ({0}, {1})".format(self.length, self.width))
        self.height = self.length // self.width
    
    def iterrows(self):
        """Iterate rows of the tile."""
        for i in range(self.height):
            yield self._vals[i * self.width: (i + 1) * self.width]
    
    def __getitem__(self, keys):
        """Getting and slicing."""
        if isinstance(keys, tuple):
            # 2D tuple slicing.
            if len(keys) != 2:
                raise NotImplementedError("3D+ dimension slicing not allowed.")
            # Unpack
            x_key, y_key = keys
            # Integers means simple 2D coordinates
            if all(isinstance(elem, int) for elem in keys):
                return self._vals[(y_key * self.width) + x_key]
            # An X slice means we're returning a row (or part of it)
            elif isinstance(x_key, slice) and isinstance(y_key, int):
                return self._vals[y_key * self.width: (y_key + 1) * self.width][x_key]
            # An Y slice means we're returning a column (or part of it)
            elif isinstance(y_key, slice) and isinstance(x_key, int):
                return self._vals[x_key: None: self.width][y_key]
            # A double slice means returning a subtile
            else:
                if y_key.step is not None and y_key != 1:
                    raise NotImplementedError("Y slicing with non-unity steps not implemented.")
                new_vals = []
                new_width = None
                y_stop = (self.height + y_key.stop) if y_key.stop < 0 else y_key.stop or self.height
                for idx, row in enumerate(self.iterrows()):
                    new_width = len(row[x_key])
                    if idx >= (y_key.start or 0) and idx < (y_stop or self.height):
                        new_vals += row[x_key]
                return self.__class__(new_vals, width=new_width)
        else:
            # 1D length slicing.
            return self._vals[keys]
    
    def _rotate(self):
        """Do a single step rotation.
        
        Using the indexing code to build up the new one.
        """
        new_vals = []
        for col in range(self.width):
            new_vals += reversed(self[col, :])
        return self.__class__(vals=new_vals, width=self.height)
    
    def copy(self):
        return self.__class__(vals=self._vals.copy(), width=self.width)
        
    def rotate(self, steps):
        new_tile = self.copy()
        for _ in range(steps % 4):
            new_tile = new_tile._rotate()
        return new_tile
        
    def __repr__(self):
        return "<Tile {0}x{1}: {2}...>".format(self.width, self.height, ','.join(str(elem) for elem in self._vals[:6]))


class TileSet:
    """A set of tiles."""
    def __init__(self, fname):
        with open(fname) as f:
            raw_tiles = f.read().split("\n\n")
        self.tiles = {}
        for raw_tile in raw_tiles:
            # Seperate off the tile number
            tile_no, _, tile_content = raw_tile.partition("\n")
            tile_no = int(tile_no[5:9])
            first_row, _, _ = tile_content.partition("\n")
            tile_content = [int(c) for c in tile_content.replace("#", '1').replace('.','0').replace('\n', '')]
            tile = Tile(tile_content, width=len(first_row))
            self.tiles[tile_no] = tile



for fname in ["020-tiles-1.txt", "020-tiles-2.txt"]:
    print("fname", fname)
    t = Tile(list(range(16)), width=4)
    print(t[0])
    print(t[0:])
    print(t[1,1])
    print(t[1:3,1:3])
    print(t[:,2])
    print(t[2,:])
    print(t[1:3,1:3].rotate(1))
    print(t[1:3,1:3].rotate(2))
    print(t[1:-1,2])
    print(t[2,1:-1])
    print(t[1:-1,1:-1])
    ts = TileSet(fname)
    print(ts.tiles)

    
