"""Day 10 asteroid maps."""

import math

class Asteroid(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.stats = {}

    def __str__(self):
        if self.stats:
            return "<Asteroid @{x},{y} {stats!r}>".format(x=self.x, y=self.y, stats=self.stats)
        else:
            return "<Asteroid @{x},{y}>".format(x=self.x, y=self.y)

    def __repr__(self):
        return str(self)

    def bearing_to(self, other):
        """Bearing and distance to other.

        Bearing is a vector in it's lowest denominator form.

        Distance is the manhattan geometry distance for simplicity.
        """
        displacement_vector = (other.x - self.x, other.y - self.y)
        manhattan_distance = sum([abs(val) for val in displacement_vector])
        while True:
            gcd = math.gcd(*displacement_vector)
            if gcd == 1 or manhattan_distance == 0:
                break
            else:
                displacement_vector = tuple(elem // gcd for elem in displacement_vector)
        return displacement_vector, manhattan_distance

class AsteroidField(object):
    def __init__(self, map_list):
        """map_list should be an iterable of strings."""
        self.map_list = map_list
        # x and y are o-indexed from top left
        self.roid_list = []
        for y, row in enumerate(self.map_list):
            for x, elem in enumerate(row):
                if elem == '#':
                    self.roid_list.append(
                        Asteroid(x, y)
                    )

    @classmethod
    def from_file(cls, fname):
        buff = []
        with open(fname, 'r') as f:
            for line in f:
                buff.append(line.strip('\n'))
        return cls(map_list=buff)
    
    def assess(self):
        best = None
        for roid_a in self.roid_list:
            buffer = {}
            for roid_b in self.roid_list:
                b, d = roid_a.bearing_to(roid_b)
                if d == 0:
                    pass
                elif b not in buffer or d < buffer[b][0]:
                    buffer[b] = (d, roid_b)
                else:
                    # This isn't closer
                    pass
            # print(roid_a, len(buffer.keys()))
            visible = len(buffer.keys())
            if best is None or visible > best[0]:
                best = (visible, roid_a)
        return best


m = AsteroidField.from_file('010-map-t.txt')

print(m.assess())
