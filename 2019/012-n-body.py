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
    def _unit_diff(a, b):
        """Compares b & a. Returns 1 if b < a."""
        if b < a:
            return 1
        elif b > a:
            return -1
        else:
            return 0

    def grav_diff(self, other):
        return self.__class__(
            self._unit_diff(other.x, self.x),
            self._unit_diff(other.y, self.y),
            self._unit_diff(other.z, self.z)
        )

    def __str__(self):
        return "<Vector: x:{0}, y:{1}, z:{2}>".format(
            self.x, self.y, self.z
        )

    def energy(self):
        return abs(self.x) + abs(self.y) + abs(self.z)


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


class System(object):
    def __init__(self, *moons):
        self.moons = moons
        self.step = 0

    def apply_gravity(self):
        for m in self.moons:
            # Yes technically we'll apply gravity from itself, but the effect
            # will be zero.
            m.apply_gravity_from(*self.moons)

    def take_step(self):
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
            raise ValueError("Cannot sim until a number that has already passed!")

        while True:
            if self.step == step:
                break
            else:
                self.take_step()

# TEST
# <x=9, y=13, z=-8>
# <x=-3, y=16, z=-17>
# <x=-4, y=11, z=-10>
# <x=0, y=-2, z=-2>

s = System(
    Moon(Vector(9, 13, -8)),
    Moon(Vector(-3, 16, -17)),
    Moon(Vector(-4, 11, -10)),
    Moon(Vector(0, -2, -2))
)

print(s)
s.sim_until(step=1000)
print(s)
