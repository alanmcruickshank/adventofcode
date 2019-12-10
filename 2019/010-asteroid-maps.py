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
        print("Operating on {0}".format(fname))
        with open(fname, 'r') as f:
            for line in f:
                buff.append(line.strip('\n'))
        return cls(map_list=buff)

    def get_visible(self, roid):
        buffer = {}
        for roid_b in self.roid_list:
            b, d = roid.bearing_to(roid_b)
            if d == 0:
                pass
            elif b not in buffer or d < buffer[b][0]:
                buffer[b] = (d, roid_b)
            else:
                # This isn't closer
                pass
        return buffer

    def assess(self):
        best = None
        for roid_a in self.roid_list:
            buffer = self.get_visible(roid_a)
            # print(roid_a, len(buffer.keys()))
            visible = len(buffer.keys())
            if best is None or visible > best[0]:
                best = (visible, roid_a)
        return best

    def iter_laser(self, roid=None, stop_at=10):
        # initialise the laser pointing vertically.
        # Bearing is defined zero at dead horizontal right and rotates
        # clockwise to +pi. if at any point there are not points
        # at a higher bearing, then we minus 2pi from it and carry on.
        # ACTUALLY - do it in fractions so we're not messed up by floating point error
        laser_bearing = (0, -1)  # -math.pi / 2
        target_n = 1

        # If roid is none, we first select the best asteroid
        if roid is None:
            _, roid = self.assess()

        while True:
            # Get the visible asteroids
            visible = self.get_visible(roid)
            # Make a list and sort by bearing FROM THE LASER
            visible = [(b, math.atan2(b[1], b[0]) - math.atan2(laser_bearing[1], laser_bearing[0]), *visible[b]) for b in visible]
            # For any bearings less than zero add 2pi
            visible = [(b, a + (2 * math.pi) if a < 0 else a, d, r) for b, a, d, r in visible]
            # Sort by angle from laser
            visible = sorted(visible, key=lambda v: v[1])
            # Identify the current target
            target = visible[0][3]
            print("Target #{0} is {1}".format(target_n, target))
            target_n += 1
            # Remove the target
            self.roid_list.remove(target)
            # Update the bearing to that of the NEXT ASTEROID BEFORE REMOVAL
            laser_bearing = visible[1][0]
            # for v in visible:
            #    print(v)
            if target_n > stop_at:
                break


m = AsteroidField.from_file('010-map-t.txt')

m.iter_laser(stop_at=200)
