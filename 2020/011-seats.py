"""Advent of Code Day 11

https://adventofcode.com/2020/day/11

Game of life anyone?
"""


inputs = [
    "011-seats-test.txt",
    "011-seats-input.txt",
]


class SeatingPlan:

    forward_mapping = {
        'L': False,
        '#': True,
        '.': None
    }

    def __init__(self, fname):
        with open(fname) as f:
            self.plan = f.readlines()
        # Extract some meta data to use for getting values.
        self._height = len(self.plan)
        # Convert to lists of bools and nones
        for i in range(self._height):
            self.plan[i] = [
                self.forward_mapping[c]
                for c in self.plan[i].strip('\n')
            ]
        # Extract the width afterward so we strip newlines properly
        self._width = len(self.plan[0])

    def get(self, x, y):
        """We're ZERO indexed. Origin is top left, and y is positive DOWN."""
        # Are we out of bounds?
        if x < 0 or y < 0 or x >= self._width or y >= self._height:
            return None
        return self.plan[y][x]
    
    @staticmethod
    def print_char(c):
        if c is None:
            return '.'
        if c is False:
            return 'L'
        if c is True:
            return '#'
        return 'x'
    
    def __str__(self):
        return '\n'.join(
            ''.join(self.print_char(c) for c in row)
            for row in self.plan
        )
    
    def occupancy_map(self):
        return '\n'.join(
            ''.join(str(self.occupancy_around(x, y)) for x in range(self._width))
            for y in range(self._height)
        )
    
    def occupancy_around(self, x, y, method='view'):
        """Calculate the occupancy around this seat."""
        offsets = [
                (-1, -1),
                (0, -1),
                (1, -1),
                (-1, 0),
                (1, 0),
                (-1, 1),
                (0, 1),
                (1, 1),
            ]
        if method == 'touch':
            return sum(
                self.get(x + dx, y + dy) or 0
                for dx, dy in offsets
            )
        elif method == 'view':
            counts = 0
            for dx, dy in offsets:
                dist = 1
                while True:
                    nx, ny = x + (dist * dx), y + (dist * dy)
                    # Have we gone too far?
                    if nx < 0 or ny < 0 or nx >= self._width or ny >= self._height:
                        v = 0
                        break
                    v = self.get(nx, ny)
                    if v is not None:
                        v = int(v)
                        break
                    dist += 1
                counts += v
            return counts
    
    def step(self, leave_lim=5):
        """Take a step."""
        # Start a new plan to evaluate the whole thing at once.
        new_plan = []
        for y in range(self._height):
            # copy it for cleanliness
            new_row = self.plan[y].copy()
            for x in range(self._width):
                cur_val = self.get(x, y)
                if cur_val is None:
                    continue
                occupancy = self.occupancy_around(x, y)
                if cur_val is True and occupancy >= leave_lim:
                    new_row[x] = False
                    continue
                if cur_val is False and occupancy == 0:
                    new_row[x] = True
                    continue
            new_plan.append(new_row)
        self.plan = new_plan
    
    def count_occupied(self):
        return sum(
            sum(self.get(x, y) or 0 for x in range(self._width))
            for y in range(self._height)
        ) 
    
    def step_till_stable(self, limit=100):
        for i in range(limit):
            plan = self.plan
            self.step()
            if plan == self.plan:
                print("STABLE! IDX: ", i, " OCCUPIED: ", self.count_occupied())
                break
        else:
            raise RuntimeError("Reached limit, not stability.")


for fname in inputs:
    print(fname)
    plan = SeatingPlan(fname)
    # print(str(plan))
    # for i in range(3):
    #     print(i, " ---------------")
    #     plan.step()
    #     print(str(plan))
    #     print("------")
    #     print(plan.occupancy_map())
    plan.step_till_stable()
    # Part 1 answer: 2321
    # Part 2 answer: 2102
