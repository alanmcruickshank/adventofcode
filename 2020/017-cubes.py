"""Advent of Code Day 17

https://adventofcode.com/2020/day/17

Game of life 3d.
"""


from collections import defaultdict


class Game:
    def __init__(self, field):
        # Assume top left origin coordinate system.
        # Coordinates are tuples.
        self._space = defaultdict(int)

        # Assume z=0 for all initial values
        z = 0
        for y, line in enumerate(field.split('\n')):
            for x, char in enumerate(line):
                val = 1 if char == '#' else 0
                coord = (x, y, z)
                self._space[coord] = val
        # Infer the current bounds
        self._infer_bounds()
    
    def _infer_bounds(self):
        # Only the bounds of ACTIVE cells matter
        x, y, z = zip(*[k for k in self._space.keys() if self._space[k] == 1])
        self._bounds = {
            'x': (min(x), max(x)),
            'y': (min(y), max(y)),
            'z': (min(z), max(z)),
        }
    
    @staticmethod
    def iter_neighbours(pos):
        for dx in range(-1, 2):
            for dy in range(-1, 2):
                for dz in range(-1, 2):
                    new_pos = (pos[0] + dx, pos[1] + dy, pos[2] + dz)
                    if new_pos != pos:
                        yield new_pos
    
    def step(self):
        # Assume that the bounds are currently true.
        # Iterate through the existing bounds.
        newspace = defaultdict(int)

        for x in range(self._bounds['x'][0] - 1, self._bounds['x'][1] + 2):
            for y in range(self._bounds['y'][0] - 1, self._bounds['y'][1] + 2):
                for z in range(self._bounds['z'][0] - 1, self._bounds['z'][1] + 2):
                    pos = (x, y, z)
                    val = self._space[pos]
                    surround = 0
                    for neighbour in self.iter_neighbours(pos):
                        surround += self._space[neighbour]
                    
                    if val == 1 and surround in (2, 3):
                        newspace[pos] = 1
                    elif val == 0 and surround == 3:
                        newspace[pos] = 1
                    else:
                        newspace[pos] = 0

        # Update to the new version        
        self._space = newspace

        # Infer the current bounds
        self._infer_bounds()
    
    def view(self):
        print("\n#########")
        for z in range(self._bounds['z'][0], self._bounds['z'][1] + 1):
            print("Z =", z)

            for y in range(self._bounds['y'][0], self._bounds['y'][1] + 1):
                line = ""
                for x in range(self._bounds['x'][0], self._bounds['x'][1] + 1):
                    if self._space[(x, y, z)] == 1:
                        line += '#'
                    else:
                        line += '.'
                print(line)
    
    def sum(self):
        return sum(self._space.values())

# Dummy example
game = Game(field='.#.\n..#\n###')
for i in range(6):
    game.step()
print(game.sum())
# Test example = 112

# Part 1
game = Game(field="""
#.##....
.#.#.##.
###.....
....##.#
#....###
.#.#.#..
.##...##
#..#.###
""")
for i in range(6):
    game.step()
print(game.sum())
# Part 1 example = 230
