"""Advent of Code Day 3

https://adventofcode.com/2020/day/3
"""

import math


class Forest:
    """A forest of trees."""

    def __init__(self, base_map):
        if set(base_map) != {'#', '.', '\n'}:
            raise ValueError("This forest doesn't look like a forest: ".format(base_map))
        self.map_lines = base_map.split('\n')
        self.width = len(self.map_lines[0])
        self.height = len(self.map_lines)
    
    @classmethod
    def from_file(cls, fname):
        with open(fname) as txt_file:
            return cls(txt_file.read())
    
    def __str__(self):
        return "<Forest: H:{0} [W:{1}]>".format(self.height, self.width)
    
    def is_tree(self, x, y):
        return self.map_lines[y][x] == '#'
    
    def iter_positions(self, right, down):
        x, y = 0, 0
        while y < self.height:
            yield x, y
            x += right
            y += down
            # Modulo width division to do wrapping
            x %= self.width
    
    def iter_trees(self, right, down):
        for x, y in self.iter_positions(right, down):
            yield self.is_tree(x, y)
    
    def evaluate_slopes(self, slopes):
        trees = []
        for r, d in slopes:
            trees.append(sum(self.iter_trees(r, d)))
        return trees, math.prod(trees)


# Load the test forest
test_forest = Forest.from_file("003-taboggan-test.txt")
print(test_forest)
print(sum(test_forest.iter_trees(3,1)))

# Load the actual forest
forest = Forest.from_file("003-taboggan-input.txt")
print(forest)
print(sum(forest.iter_trees(3,1)))
# Answer 237


# ### Part 2
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

# Test
print(test_forest.evaluate_slopes(slopes))

# Real
print(forest.evaluate_slopes(slopes))
# ANSWER = 2106818610
