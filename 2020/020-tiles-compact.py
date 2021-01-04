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
This could be done using a class, but functions
will be swifter here.
"""

from collections import defaultdict
from itertools import permutations, product


DIRECTIONS = [
    (i, complex(0, 1)**i) for i in range(4)
]


def direction_of(c):
    unit = c / abs(c)
    for d, test_c in DIRECTIONS:
        if unit == test_c:
            return d
    else:
        raise ValueError("Direction not found.")


class BinaryGrid:
    def __init__(self, rows, row_bit_length=None):
        # Assume rows is an array of integers.
        self._rows = rows
        self.height = len(self._rows)
        self.width = row_bit_length or max(self.bitlen(row) for row in self._rows)
    
    @staticmethod
    def bitlen(int_type):
        """Get the bit length of an integer."""
        length = 0
        while int_type:
            int_type >>= 1
            length += 1
        return length
    
    @staticmethod
    def posbits(int_type):
        """Counts the positive bit of an integer."""
        count = 0
        while int_type:
            count += int_type & 1
            int_type >>= 1
        return count
    
    def col(self, idx, inv=False):
        """Get a column as an integer.
        
        Reads bits vertically downward (unless inverted).
        """
        val = 0
        for i in range(self.height):
            bit = i if inv else self.height - i
            val += ((self._rows[i] >> (self.width - idx)) % 2) * (2**bit)
        return val
    
    def row(self, idx, inv=False):
        """Get a row as an integer.
        
        Reads bits vertically downward (unless inverted).
        """
        val = self._rows[idx]
        if inv:
            print([((val >> (self.width - bit)) % 2) * (2**bit) for bit in range(self.width)])
            return sum(((val >> (self.width - bit)) % 2) * (2**bit) for bit in range(self.width))
        else:
            return val


for i in [1,2,3,4,5,6,7,8,32,128]:
    print(i, BinaryGrid.bitlen(i), BinaryGrid.posbits(i))

# 111
# 010
# 110

g = BinaryGrid(rows=[7, 2, 6])
print(g.width)

for i in range(3):
    print(i, g.row(i))

for i in range(3):
    print(i, g.col(i))

for i in range(3):
    print(i, g.row(i, inv=True))

for i in range(3):
    print(i, g.col(i, inv=True))

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

    def get_key_at(self, side, rot, hflip, vflip):
        # first filter to the relevant items
        filtered_keys = [key for key in self.keys if key['hflip'] == hflip and key['vflip'] == vflip]
        for key in filtered_keys:
            if key['side'] == (side - rot) % 4:
                return key
        else:
            raise RuntimeError("Unable to find key!?")

    @staticmethod
    def tilestring_to_int(s):
        return int(s.replace('#', '1').replace('.', '0'), 2)
    
    def __repr__(self):
        return "<Tile {0}: {1}>".format(self.tile_no, self.rows[0][:5])
    
    @staticmethod
    def rotate_rows(rows):
        """rotates by one step."""
        new_rows = []
        for x in range(len(rows[0])):
            row = ""
            for y in reversed(range(len(rows))):
                row += rows[y][x]
            new_rows.append(row)
        return new_rows
    
    def oriented_content(self, r=0, hflip=False, vflip=False, trim=True):
        rows = self.rows
        # Trim the border
        if trim:
            rows = [row[1:-1] for row in rows[1:-1]]
        # Horizontal Flip
        if hflip:
            rows = [row[::-1] for row in rows]
        # Vertical Flip
        if vflip:
            rows = [row for row in reversed(rows)]
        # Rotate (as many times as required)
        for _ in range(r % 4):
            rows = self.rotate_rows(rows)
        return rows


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
    
    def link(self, pruned_links):
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
        pruned_links = self.pruned_links()
        neighbors = self.link(pruned_links)
        return self.product(self.find_corners(neighbors))
    
    def position(self, neighbors):
        corners = self.find_corners(neighbors)
        # Start in the first corner, building up an image.
        # First position all the tiles on a grid.
        positioned_tiles = {}
        cur_tile = corners[0]
        pos = complex(0, 0)
        positioned_tiles[cur_tile] = pos
        # Work along one side (assuming it's the top)
        step = complex(1, 0)
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
                # NB: Multiplication here is like a rotation
                step *= complex(0, -1)
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
                potential_points = None
                # Get all the touching points for a point and then intersect it with the set of potential points
                for n in placed_neighbors[cur_tile]:
                    touching_points = set([positioned_tiles[n] + (complex(0, 1)**r) for r in range(4)])
                    if potential_points:
                        potential_points &= touching_points
                    else:
                        potential_points = touching_points
                # Take the first one not already taken.
                for point in potential_points:
                    if point not in positioned_tiles.values():
                        pos = point
                        break
                else:
                    raise RuntimeError("No appropriate point found!")
            else:
                # Nothing to place!
                break

            positioned_tiles[cur_tile] = pos

        return positioned_tiles
    
    @staticmethod
    def invert_position_map(positions):
        return {pos: tile for tile, pos in positions.items()}

    def orient(self, positions, pruned_links, neighbors):
        oriented_tiles = {}
        # We don't have to orient in an order, because the positions
        # determine the orientation.
        invert_grid = self.invert_position_map(positions)
        for tile_no in positions:
            local_neighbor_positions = {t: positions[t] for t in neighbors[tile_no]}
            local_neighbor_dirs = {t: local_neighbor_positions[t] - positions[tile_no] for t in neighbors[tile_no]}
            # Plus one to line up with different cardinal directions.
            local_neighbor_sides = {(direction_of(local_neighbor_dirs[t]) + 1) % 4: t for t in neighbors[tile_no]}

            local_links = []
            for key in pruned_links:
                for link in pruned_links[key]:
                    if link['tile'].tile_no == tile_no:
                        local_links.append(link)

            # Try orientations until we find one that fits.
            for r, hflip, vflip in product(range(4), [False, True], [False, True]):
                for s in range(4):
                    # Do we actually have a neighbor on that side.
                    if s not in local_neighbor_sides:
                        continue
                    side_key = self.tiles[tile_no].get_key_at(s, r, hflip, vflip)
                    # Is this a match with the keys of the neighbor tile?
                    for key in pruned_links.get(side_key['key'], []):
                        if key['tile'].tile_no == local_neighbor_sides[s]:
                            break
                    else:
                        # We couldn't find a matching key.
                        break
                else:
                    # We got to the end! it's a match.
                    oriented_tiles[tile_no] = {'r': r, 'hflip': hflip, 'vflip': vflip}
                    break
            else:
                raise RuntimeError("Could not orient tile!")
        return oriented_tiles
    
    def print_grid(self):
        pruned_links = self.pruned_links()
        neighbors = self.link(pruned_links)
        positions = self.position(neighbors)
        orientation = self.orient(positions, pruned_links, neighbors)
        # Work out extents
        extents = [
            (min(int(elem.real) for elem in positions.values()), max(int(elem.real) for elem in positions.values())),
            (min(int(elem.imag) for elem in positions.values()), max(int(elem.imag) for elem in positions.values()))
        ]
        invert_grid = self.invert_position_map(positions)

        oriented_tiles = {tn: self.tiles[tn].oriented_content(trim=True, **orientation[tn]) for tn in orientation}  #**orientation[tn]

        # NB: Trimmed tiles, so range - 2
        tile_width = self.tile_size[1] - 2

        raw_rows = []
        for grid_y in range(extents[1][0], extents[1][1] + 1):
            tile_no_row = " "
            for grid_x in range(extents[0][0], extents[0][1] + 1):
                tile_no_row += str(invert_grid[complex(grid_x, grid_y)]).ljust(tile_width) + " "
            print(tile_no_row)
            for row_idx in range(tile_width):
                row_buff = " "
                raw_row_buff = ""
                for grid_x in range(extents[0][0], extents[0][1] + 1):
                    tile_no = invert_grid[complex(grid_x, grid_y)]
                    tile_row = oriented_tiles[tile_no][row_idx]
                    row_buff += tile_row + " "
                    raw_row_buff += tile_row
                print(row_buff)
                raw_rows.append(raw_row_buff)
        return raw_rows

    
    @staticmethod
    def product(iterable):
        prod = 1
        for elem in iterable:
            prod *= elem
        return prod


seamonster = [
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   "
]


def search_for_seamonsters(grid):
    grid_w = len(grid[0])
    grid_h = len(grid)
    sm_w = len(seamonster[0])
    sm_h = len(seamonster)

    total_hashes = sum(
        row.count('#') for row in grid
    )

    monster_hashes = sum(
        row.count('#') for row in seamonster
    )

    monsters = []
    for x in range(grid_w - sm_w):
        for y in range(grid_h - sm_h):
            for dx in range(sm_w):
                for dy in range(sm_h):
                    if seamonster[dy][dx] != '#':
                        continue
                    elif grid[y + dy][x + dx] != '#':
                        # No match with seamonsters.
                        break
                else:
                    # We only get here if this row finished completely.
                    continue
                # We only get here if we failed a match.
                break
            else:
                monsters.append((x, y))
    # How many monsters did we find?
    return total_hashes, monsters, total_hashes - (len(monsters) * monster_hashes)


for fname in ["020-tiles-1.txt", "020-tiles-2.txt"]:
    print("fname", fname)
    ts = TileSet(fname)
    print(ts.find_corner_product())
    # Answer part 1: 59187348943703
    combined_grid = ts.print_grid()
    print("\n\n")
    for r in range(4):
        _, monster_locs, roughness = search_for_seamonsters(combined_grid)
        if monster_locs:
            print("Found", len(monster_locs), "monsters:", monster_locs, "R", r, "roughness", roughness)
            # Answer part 2: 1565  (with 36 monsters)
            for row in combined_grid:
                print(row)
            print("\n\n")
        # Rotate for next time.
        combined_grid = Tile.rotate_rows(combined_grid)
