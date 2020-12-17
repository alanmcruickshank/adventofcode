"""Advent of Code Day 17

https://adventofcode.com/2020/day/17

Game of life 3d.
"""


from collections import defaultdict
from itertools import product


class Game:
    def __init__(self, field, dimensions=3):
        # Assume top left origin coordinate system.
        # Coordinates are tuples.
        self._space = defaultdict(int)
        self.dimensions = dimensions

        # Assume 0 for all other initial dimensions
        for y, line in enumerate(field.split('\n')):
            for x, char in enumerate(line):
                val = 1 if char == '#' else 0
                coord = (x, y) + ((0,) * (dimensions - 2))
                self._space[coord] = val
        # Infer the current bounds
        self._infer_bounds()
    
    def _infer_bounds(self):
        # Only the bounds of ACTIVE cells matter
        axes = list(zip(*[k for k in self._space.keys() if self._space[k] == 1]))
        self._bounds = {
            axis: (min(axes[axis]), max(axes[axis]))
            for axis in range(self.dimensions)
        }
    
    def iter_neighbours(self, pos):
        for delta_pos in product(range(-1, 2), repeat=self.dimensions):
            new_pos = tuple(pos[axis] + delta_pos[axis] for axis in range(self.dimensions))
            if new_pos != pos:
                yield new_pos
    
    def step(self):
        # Assume that the bounds are currently true.
        # Iterate through the existing bounds.
        newspace = defaultdict(int)

        for pos in product(*[range(self._bounds[axis][0] - 1, self._bounds[axis][1] + 2) for axis in range(self.dimensions)]):
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
    
    def iter_non_2d_dims(self):
        if self.dimensions == 3:
            for z in range(self._bounds[2][0], self._bounds[2][1] + 1):
                yield (z,)
        else:
            for pos in product(*[range(self._bounds[axis][0], self._bounds[axis][1] + 1) for axis in range(2, self.dimensions)]):
                yield pos
    
    def view(self):
        print("\n#########")
        for other_dims in self.iter_non_2d_dims():
            print("Z =", other_dims)

            for y in range(self._bounds[1][0], self._bounds[1][1] + 1):
                line = ""
                for x in range(self._bounds[0][0], self._bounds[0][1] + 1):
                    if self._space[(x, y, *other_dims)] == 1:
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

game = Game(field='.#.\n..#\n###', dimensions=4)
for i in range(6):
    game.step()
print(game.sum())
# Test example = 848

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

# Part 2
game = Game(field="""
#.##....
.#.#.##.
###.....
....##.#
#....###
.#.#.#..
.##...##
#..#.###
""", dimensions=4)
for i in range(6):
    game.step()
print(game.sum())
# Part 2 example = 1600
