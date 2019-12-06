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

