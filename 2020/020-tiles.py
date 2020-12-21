"""Advent of Code Day 20

https://adventofcode.com/2020/day/20

Image borders.

Binary?
"""

from collections import defaultdict
from itertools import permutations, product


class Vector:
    def __init__(self, *vals):
        self._vals = tuple(vals)
    
    def __len__(self):
        return len(self._vals)

    def __repr__(self):
        return "<Vector[{0}] {1!r}>".format(len(self), self._vals)
    
    def __eq__(self, other):
        if len(self) != len(other):
            return False
        return all(self._vals[n] == other._vals[n] for n in range(len(self._vals)))
    
    def __hash__(self):
        return hash(self._vals)
    
    def copy(self):
        return self.__class__(*self._vals)
    
    def rotate(self, r):
        if r % 4 == 0:
            return self.copy()
        elif r % 4 == 1:
            return self.__class__(self._vals[1], -self._vals[0])
        elif r % 4 == 2:
            return self.__class__(-self._vals[0], -self._vals[1])
        elif r % 4 == 3:
            return self.__class__(-self._vals[1], self._vals[0])

    def __add__(self, other):
        return self.__class__(*[
            self._vals[idx] + other._vals[idx] for idx in range(len(self._vals))
        ])
    
    def to_tuple(self):
        return self._vals

    @classmethod
    def unit(cls, direction=0):
        """Unit vector points at x=1, y=0"""
        v = cls(1, 0)
        return v.rotate(direction)
    
    @classmethod
    def intersection_of(cls, points):
        point_set = set(points)
        point_list = list(point_set)
        dimensions = len(point_list[0])
        if len(point_set) in (0, 1) or len(point_set) > 4:
            raise ValueError("Inappropriate number of points for intersection: {0}".format(len(point_set)))
        else:
            # Do any differ by anything other than 0 or 2?
            p = []
            for dim in range(dimensions):
                opts_list = sorted(set(p[dim] for p in point_set))
                if len(opts_list) not in (1, 2, 3):
                    raise ValueError("Inappropriate options. {0} {1}".format(dim, opts_list))
                if len(opts_list) == 3:
                    # remove the middle one.
                    opts_list.pop(1)
                if len(opts_list) == 2:
                    if abs(opts_list[0] - opts_list[1]) == 1:
                        # We're going to have to deal with options
                        p.append(opts_list)
                    elif abs(opts_list[0] - opts_list[1]) != 2:
                        raise ValueError("Inappropriate difference. {0} {1} {2}".format(dim, opts_list, point_set))
                    else:
                        p.append([(opts_list[0] + opts_list[1]) // 2])
                else:
                    p.append([opts_list[0]])

        # Yield all the options.
        for coords in product(*p):
            point = cls(*coords)
            if point.to_tuple() not in point_set:
                yield point

class Tile:
    def __init__(self, raw):
        self.raw = raw.strip()
        self.rows = []
        for line in self.raw.split('\n'):
            if line.startswith("Tile "):
                self.tile_no = int(line.partition(":")[0][5:])
            else:
                self.rows.append(line.strip())
        
        self.size = (len(self.rows[0]), len(self.rows))
        
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
            self.tile_size = tile.size
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
    
    def link(self):
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

    @staticmethod
    def find_corners(neighbors):
        corners = []
        for key in neighbors:
            if len(neighbors[key]) == 2:
                corners.append(key)
        return corners

    def find_corner_product(self):
        neighbors = self.link()
        return self.product(self.find_corners(neighbors))
    
    def position(self):
        neighbors = self.link()
        corners = self.find_corners(neighbors)
        # Start in the first corner, building up an image.
        # First position all the tiles on a grid.
        positioned_tiles = {}
        cur_tile = corners[0]
        pos = Vector(0, 0)
        positioned_tiles[cur_tile] = pos.to_tuple()
        # Work along one side (assuming it's the top)
        step = Vector.unit()
        while True:
            # find links to the corner tile.
            linked_tiles = neighbors[cur_tile]
            # Classify those linked tiles.
            already_placed_links = []
            linked_corners = []
            linked_edges = []
            linked_bulk = []
            for tile_no in linked_tiles:
                if tile_no in positioned_tiles:
                    already_placed_links.append(tile_no)
                elif len(neighbors[tile_no]) == 2:
                    linked_corners.append(tile_no)
                elif len(neighbors[tile_no]) == 3:
                    linked_edges.append(tile_no)
                else:
                    linked_bulk.append(tile_no)

            # If we have a corner, place it and rotate.
            if linked_corners:
                cur_tile = linked_corners[0]
                pos += step
                step = step.rotate(1)
            # If we have edges, place one and move on.
            elif linked_edges:
                cur_tile = linked_edges[0]
                pos += step
            # No corners or edges
            elif linked_bulk:
                # Of the linked bulk tiles, find the one with the most
                # placed neighbors.
                placed_neighbors = {
                    t: [l for l in neighbors[t] if l in positioned_tiles]
                    for t in linked_bulk
                }
                _, cur_tile = sorted(((len(val), key) for key, val in placed_neighbors.items()), reverse=True)[0]
                # Use the position of the others to place this tile.
                potential_points = list(Vector.intersection_of([positioned_tiles[n] for n in placed_neighbors[cur_tile]]))
                for point in potential_points:
                    if point.to_tuple() not in positioned_tiles.values():
                        pos = point
                        break
                else:
                    raise RuntimeError("No appropriate point found!")
            else:
                # Nothing to place!
                break

            positioned_tiles[cur_tile] = pos.to_tuple()
            # print("placed", cur_tile , pos.to_tuple())

        return positioned_tiles
    
    def print_grid(self):
        positions = self.position()
        # Work out extents
        extents = [
            (min(elem[0] for elem in positions.values()), max(elem[0] for elem in positions.values())),
            (min(elem[1] for elem in positions.values()), max(elem[1] for elem in positions.values()))
        ]
        invert_grid = {pos: tile for tile, pos in positions.items()}

        for grid_y in range(extents[1][0], extents[1][1] + 1):
            print(" ")
            for row_idx in range(self.tile_size[1]):
                row_buff = " "
                for grid_x in range(extents[0][0], extents[0][1] + 1):
                    tile_no = invert_grid[(grid_x, grid_y)]
                    tile = self.tiles[tile_no]
                    tile_row = tile.rows[row_idx]
                    row_buff += tile_row + " "
                print(row_buff)

    
    @staticmethod
    def product(iterable):
        prod = 1
        for elem in iterable:
            prod *= elem
        return prod

for fname in ["020-tiles-1.txt", "020-tiles-2.txt"]:
    print("fname", fname)
    ts = TileSet(fname)
    print(ts.find_corner_product())
    # Answer part 1: 59187348943703
    ts.print_grid()
    print("\n\n")

