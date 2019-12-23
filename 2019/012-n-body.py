"""Day 12 - N Body Problem."""


class Vector(object):
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

    def __add__(self, other):
        return self.__class__(
            x=self.x + other.x,
            y=self.y + other.y,
            z=self.z + other.z)

    @staticmethod
    def unit_diff(a, b):
        """Compares b & a. Returns 1 if b < a."""
        if b < a:
            return 1
        elif b > a:
            return -1
        else:
            return 0

    def grav_diff(self, other):
        return self.__class__(
            self.unit_diff(other.x, self.x),
            self.unit_diff(other.y, self.y),
            self.unit_diff(other.z, self.z)
        )

    def __str__(self):
        return "<Vector: x:{0}, y:{1}, z:{2}>".format(
            self.x, self.y, self.z
        )

    def energy(self):
        return abs(self.x) + abs(self.y) + abs(self.z)

    def to_tuple(self):
        return (self.x, self.y, self.z)

    def __getitem__(self, val):
        return (self.x, self.y, self.z)[val]


class Moon(object):
    def __init__(self, pos):
        self.pos = pos
        self.vel = Vector(0, 0, 0)

    def apply_gravity_from(self, *moons):
        for m in moons:
            self.vel += self.pos.grav_diff(m.pos)

    def take_vel_step(self):
        self.pos += self.vel

    def __str__(self):
        return "<Moon @{0}, vel:{1}>".format(self.pos, self.vel)

    def energy(self):
        return self.pos.energy() * self.vel.energy()

    def to_tuple(self):
        return (self.pos.to_tuple(), self.vel.to_tuple())


class System(object):
    def __init__(self, *moons):
        self.moons = moons
        self.step = 0
        self.prev_states = set()

    def apply_gravity(self):
        for m in self.moons:
            # Yes technically we'll apply gravity from itself, but the effect
            # will be zero.
            m.apply_gravity_from(*self.moons)

    def take_step(self):
        self.prev_states.add(self.to_tuple())
        self.apply_gravity()
        for m in self.moons:
            m.take_vel_step()
        self.step += 1

    def __str__(self):
        energy = sum([m.energy() for m in self.moons])
        buff = "<System, steps:{0}, energy:{1}>\n".format(self.step, energy)
        for m in self.moons:
            buff += "    {0}\n".format(m)
        return buff

    def sim_until(self, step=100):
        if self.step > step:
            raise ValueError(
                "Cannot sim until a number that has already passed!")

        while True:
            if self.step == step:
                break
            else:
                self.take_step()

    def sim_until_prev(self):
        while True:
            if self.to_tuple() in self.prev_states:
                break
            else:
                self.take_step()

    def to_tuple(self):
        return tuple([m.to_tuple() for m in self.moons])

    @classmethod
    def from_iterable(cls, moons_structs):
        moon_objs = []
        for m in moons_structs:
            moon_objs.append(Moon(Vector(*m)))
        return cls(*moon_objs)

    @staticmethod
    def step_1d(world):
        world_buffer = []
        # Apply Gravity, then velocity
        for m in world:
            # iterate each of the subjects
            dv = 0
            for t in world:
                # iterate each of the references
                dv += Vector.unit_diff(t[0], m[0])
            v = m[1] + dv
            world_buffer.append((m[0] + v, v))
        return tuple(world_buffer)

    def analyse_axis(self, idx):
        """Analyse the offset and period for a given axis."""

        world_state = tuple([
            (m.pos[idx], m.vel[idx])
            for m in self.moons
        ])
        step = self.step
        prev_states = {}
        while True:
            if world_state in prev_states:
                # We've found a recurrance, return the first and
                # second occurrance
                return prev_states[world_state], step
            else:
                # No recurrance (take another step)
                prev_states[world_state] = step
                world_state = self.step_1d(world_state)
                step += 1

    @staticmethod
    def prime_factors(val):
        """From https://stackoverflow.com/questions/15347174/python-finding-prime-factors"""  # noqa
        n = val
        i = 2
        factors = {}
        while i * i < n:
            while n % i == 0:
                if i in factors:
                    factors[i] += 1
                else:
                    factors[i] = 1
                n = n / i
            i = i + 1
        factors[int(n)] = 1
        return factors

    def common_period(self, *periods):
        """Find the common period of three periods."""
        factors = [self.prime_factors(p) for p in periods]
        # We want to use each factor only as many times as it appears in *any*.
        unique_factors = set.union(*[set(f.keys()) for f in factors])
        final_factors = {
            k: max([f.get(k, 0) for f in factors])
            for k in unique_factors
        }
        period = 1
        for k in final_factors:
            period *= k ** final_factors[k]
        return period

    def analyse(self):
        """Decompose each of the axes, simulate seperately.

        Each axis is effectively independent, first we have to find each of
        their periods and offsets and then we can effectively fast forward to
        find their intersection.
        """
        analysis_buff = []
        for i in range(0, 3):
            analysis_buff.append(self.analyse_axis(i))

        if any([axis[0] != 0 for axis in analysis_buff]):
            raise ValueError(
                "One of the axes has a non-zero offset! {0!r}".format(
                    analysis_buff))

        # Divide by common factors
        period = self.common_period(*[axis[1] for axis in analysis_buff])
        return analysis_buff, period


example_struct = [
    (-1, 0, 2),
    (2, -10, -7),
    (4, -8, 8),
    (3, 5, -1)
]

test_struct = [
    (9, 13, -8),
    (-3, 16, -17),
    (-4, 11, -10),
    (0, -2, -2)
]

s = System.from_iterable(test_struct)
print(s)
print(s.analyse())
# Answer is 354540398381256
