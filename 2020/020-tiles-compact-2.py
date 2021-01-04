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

    direction_slices = {1j: (slice(None), 0), -1: (0, slice(None)), -1j: (slice(None), -1), 1: (-1, slice(None))}

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
                y_key = self.height + y_key if y_key < 0  else y_key  # Deal with potential negative
                return self._vals[y_key * self.width: (y_key + 1) * self.width][x_key]
            # An Y slice means we're returning a column (or part of it)
            elif isinstance(y_key, slice) and isinstance(x_key, int):
                x_key = self.width + x_key if x_key < 0  else x_key  # Deal with potential negative
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
    
    def _flip(self):
        """Flips vertically.
        
        NB: We only need to implement one flip, because we can get the other with rotation."""
        new_vals = []
        for row in reversed(list(self.iterrows())):
            new_vals += row
        return self.__class__(new_vals, width=self.width)
    
    def copy(self):
        return self.__class__(vals=self._vals.copy(), width=self.width)
        
    def rotate(self, steps):
        new_tile = self.copy()
        for _ in range(steps % 4):
            new_tile = new_tile._rotate()
        return new_tile
    
    def iter_transforms(self):
        """Iterate through all the potential orientations."""
        new_tile = self.copy()
        for _ in range(4):
            yield new_tile
            new_tile = new_tile._rotate()
        new_tile = new_tile._flip()
        for _ in range(4):
            yield new_tile
            new_tile = new_tile._rotate()
    
    def sides(self):
        return {key: self[self.direction_slices[key]] for key in self.direction_slices}
    
    def side(self, side):
        """Get the side (as defined by a complex direction)"""
        return self[self.direction_slices[side]]
        
    def __repr__(self):
        return "<Tile {0}x{1}: {2}...>".format(self.width, self.height, ','.join(str(elem) for elem in self._vals[:6]))


class TileSet:
    """A set of tiles.
    
    Tiles are stored in a dict referencing their number.
    """
    # Define a seamonster
    seamonster = Tile(
        [int(elem) for elem in "000000000000000000101000011000011000011101001001001001001000"],
        width=20
    )

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

    @staticmethod
    def calc_open_positions(tile_positions):
        open_positions = set()
        # Work out all the neighbors
        for pos in tile_positions:
            for d in DIRECTIONS:
                open_positions.add(pos + d)
        # Remove any taken positions
        open_positions -= set(tile_positions.keys())
        return open_positions

    def position(self):
        """Position tiles, starting at an arbitrary point."""
        # Make a buffer of tiles to work through.
        available_tiles = self.tiles.copy()
        # Pick a tile to start with arbitrarily, and put it at the origin.
        starting_tile_no, _ = available_tiles.popitem()
        # Keep track of which tiles we've tried in which positions.
        tried_positions = defaultdict(list)
        # Keep track of any positions we've set.
        tile_positions = {complex(0, 0): starting_tile_no}
        # Loop until golden brown.
        while available_tiles:
            open_positions = self.calc_open_positions(tile_positions)
            # Work out the constraints on each open position
            open_constraints = defaultdict(dict)
            for open_pos in open_positions:
                for d in DIRECTIONS:
                    if open_pos + d in tile_positions:
                        open_constraints[open_pos][d] = self.tiles[tile_positions[open_pos + d]].side(-d)
            # Pick a tile.
            for tile_no, tile in available_tiles.items():
                # Pick an open position.
                for pos in open_positions:
                    # Have we tried this already?
                    if pos in tried_positions[tile_no]:
                        print("a")
                        continue
                    # Try each orientation
                    for oriented_tile in tile.iter_transforms():
                        # Does it match?
                        sides = oriented_tile.sides()
                        for d in open_constraints[pos]:
                            if open_constraints[pos][d] != sides[d]:
                                # Not match
                                break
                        else:
                            # All match!
                            matched_tile = oriented_tile
                            matched_pos = pos
                            break
                        # Try next orientation.
                        continue
                    else:
                        # No match, next position...
                        continue
                    # found match!
                    break
                else:
                    # No match, try another tile
                    continue
                # Found a match!
                break
            else:
                raise RuntimeError("No match found!? Can't make a move.")
            # Orient the saved tile
            self.tiles[tile_no] = matched_tile
            del available_tiles[tile_no]
            tile_positions[matched_pos] = tile_no

        # Normalise so that the bottom left is at 0,0
        offset = complex(min(e.real for e in tile_positions), min(e.imag for e in tile_positions))
        tile_positions = {key - offset: tile_positions[key] for key in tile_positions}
        corner = complex(max(e.real for e in tile_positions), max(e.imag for e in tile_positions))
        return {
            'positions': tile_positions,
            'corner': corner,
            'corner_product': tile_positions[0] * tile_positions[corner] * tile_positions[corner.real] * tile_positions[corner.imag * 1j]
        }


for fname in ["020-tiles-1.txt", "020-tiles-2.txt"]:
    print("fname", fname)
    ts = TileSet(fname)
    positions = ts.position()
    print("Part 1 Answer:", positions['corner_product'])
    # Answer part 1: 59187348943703
    #print(ts.seamonster)
    #for row in ts.seamonster.iterrows():
    #    print(''.join(str(elem) for elem in row))
