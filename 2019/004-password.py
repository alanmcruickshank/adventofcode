"""Day 4 - secure container."""


def make_intlist(n):
    buff = []
    while True:
        buff.append(n % 10)
        n = n // 10
        if n == 0:
            break
    buff.reverse()
    return buff


def strictly_decreasing(n):
    """return (True, N) if strictly decreasing, return (False, N)
    if not, where N is the next number that IS strictly decreasing."""
    ln = make_intlist(n)
    # We'll build up the number in this buffer a digit at a
    # time, by x10 each time
    b = 0
    success = True
    for d in ln:
        if b:
            # Compare the digit to the last digit of the buffer
            if d >= b % 10:
                b = d + (b * 10)
            else:
                # We decreased
                success = False
                # The next best thing is to add the same number as
                # the previous digit
                b = (b % 10) + (b * 10)
        else:
            # In the first time round, we just set the buffer to
            # the first digit
            b = d
    return (success, b)


def has_pair(n):
    ln = make_intlist(n)
    runs = []
    runlen = 1
    for i in range(1, len(ln)):
        if ln[i] == ln[i - 1]:
            runlen += 1
        else:
            if runlen > 1:
                runs.append(runlen)
            runlen = 1
    if runlen > 1:
        runs.append(runlen)
    return 2 in runs


def generate_options(start, end):
    i = start
    while True:
        s, n = strictly_decreasing(i)
        if s:
            if has_pair(n):
                yield i
            i += 1
        else:
            i = n
        if i > end:
            break


opts = list(generate_options(168630, 718098))
print(len(opts))
