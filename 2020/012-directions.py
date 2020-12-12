"""Advent of Code Day 12

https://adventofcode.com/2020/day/12

Manhattan distance
"""


class Vector:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    def __add__(self, other):
        return self.__class__(
            self.x + other.x,
            self.y + other.y
        )
    
    def __mul__(self, other):
        """Multiply by a scalar."""
        return self.__class__(self.x * other, self.y * other)
    
    def __str__(self):
        return "<Vec {x}, {y}>".format(x=self.x, y=self.y)
    
    def copy(self):
        return self.__class__(self.x, self.y)
    
    def rotate(self, degrees):
        """Rotate clockwise by the number of degress."""
        if degrees % 90 != 0:
            raise ValueError("Can't rotate by a number of degrees which isn't a multiple of 90")
        # How many steps
        steps = degrees // 90
        # Anything more than 4 and we end up back in the same place
        steps %= 4
        if steps == 0:
            return self.copy()
        elif steps == 1:
            return self.__class__(self.y, -self.x)
        elif steps == 2:
            return self.__class__(-self.x, -self.y)
        elif steps == 3:
            return self.__class__(-self.y, self.x)
    
    def manhattan_distance(self):
        return abs(self.x) + abs(self.y)


class Ferry:
    def __init__(self):
        self.pos = Vector(0, 0)
        self.dir = Vector(1, 0)
    
    def __str__(self):
        return "<Ferry Pos: {0}, Dir: {1}>".format(self.pos, self.dir)

    def _move(self, action, val):
        rotations = {
            'L': -1,
            'R': 1,
        }
        translations = {
            'N': Vector(0, 1),
            'S': Vector(0, -1),
            'E': Vector(1, 0),
            'W': Vector(-1, 0),
            'F': self.dir,
        }
        if action in rotations:
            self.dir = self.dir.rotate(val * rotations[action])
        elif action in translations:
            self.pos += translations[action] * val
        else:
            ValueError("Unexpected action: {0}".format(action))

    def move(self, instruction):
        action = instruction[0]
        val = int(instruction[1:])
        self._move(action, val)


with open("012-directions-input.txt") as txt_file:
    instruction_sets = [
        "F10,N3,F7,R90,F11".split(','),
        txt_file.readlines()
    ]

for idx, inst_set in enumerate(instruction_sets):
    print("Set ", idx + 1)
    ferry = Ferry()
    for inst in inst_set:
        ferry.move(inst)
    print(ferry)
    print(ferry.pos.manhattan_distance())