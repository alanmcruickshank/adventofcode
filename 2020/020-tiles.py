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
    
    def __sub__(self, other):
        return self.__class__(*[
            self._vals[idx] - other._vals[idx] for idx in range(len(self._vals))
        ])
    
    def to_tuple(self):
        return self._vals

    @classmethod
    def unit(cls, direction=0):
        """Unit vector points at x=1, y=0"""
        v = cls(1, 0)
        return v.rotate(direction)
    
    def direction(self):
        """inverse of unit"""
        if self._vals[0] > 0 and self._vals[1] == 0:
            return 0
        elif self._vals[0] < 0 and self._vals[1] == 0:
            return 2
        elif self._vals[1] > 0 and self._vals[0] == 0:
            return 1
        elif self._vals[1] < 0 and self._vals[0] == 0:
            return 3
        else:
            raise ValueError("Inappropriate vector to call direction on...")

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
            local_neighbor_dirs = {t: Vector(*local_neighbor_positions[t]) - Vector(*positions[tile_no]) for t in neighbors[tile_no]}
            # Plus one to line up with different cardinal directions.
            local_neighbor_sides = {(local_neighbor_dirs[t].direction() + 1) % 4: t for t in neighbors[tile_no]}

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
            (min(elem[0] for elem in positions.values()), max(elem[0] for elem in positions.values())),
            (min(elem[1] for elem in positions.values()), max(elem[1] for elem in positions.values()))
        ]
        invert_grid = self.invert_position_map(positions)

        oriented_tiles = {tn: self.tiles[tn].oriented_content(trim=True, **orientation[tn]) for tn in orientation}  #**orientation[tn]

        # NB: Trimmed tiles, so range - 2
        tile_width = self.tile_size[1] - 2

        raw_rows = []
        for grid_y in range(extents[1][0], extents[1][1] + 1):
            tile_no_row = " "
            for grid_x in range(extents[0][0], extents[0][1] + 1):
                tile_no_row += str(invert_grid[(grid_x, grid_y)]).ljust(tile_width) + " "
            print(tile_no_row)
            for row_idx in range(tile_width):
                row_buff = " "
                raw_row_buff = ""
                for grid_x in range(extents[0][0], extents[0][1] + 1):
                    tile_no = invert_grid[(grid_x, grid_y)]
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
