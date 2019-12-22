"""Day 6 orbital maps."""


def read_map(fname):
    buff = []
    with open(fname, 'r') as f:
        for line in f:
            a, b = line.strip('\n').split(')')
            buff.append((a, b))
    return buff


def direct_orbits(buff):
    dmap = {}
    for a, b in buff:
        dmap[b] = a
    return dmap


def count_orbits(m):
    keys = list(m.keys())
    orbs = {}
    for k in keys:
        i = k
        o = 0
        while True:
            if i == 'COM':
                break
            else:
                i = m[i]
                o += 1
        orbs[k] = o
    return orbs


# We want an inverted tree not a normal one

r = read_map('006-map-2.txt')

m = direct_orbits(r)
# print(m)
c = count_orbits(m)
# print(c)
s = sum([c[k] for k in c])
print(s)

# ## Part 2 - orbital transfers

# to get to santa, we need to work out the common
# parent, then go up to that and then down to santa.


def path_to_com(m, e):
    """returns a list of stops on the way from e to com."""
    buff = []
    i = e
    while True:
        if i == 'COM':
            break
        else:
            i = m[i]
            buff.append(i)
    return buff


def common_parent_hops(m, e1, e2):
    p1 = path_to_com(m, e1)
    p2 = path_to_com(m, e2)
    p1.reverse()
    p2.reverse()
    i = 0
    common = None
    while True:
        if p1[i] == p2[i]:
            i += 1
        else:
            common = p1[i - 1]
            break
    # common is the common element.
    d1 = len(p1) - i
    d2 = len(p2) - i
    return common, d1, d2, d1 + d2


# print(path_to_com(m, 'SAN'))
# print(path_to_com(m, 'YOU'))

print(common_parent_hops(m, 'SAN', 'YOU'))
