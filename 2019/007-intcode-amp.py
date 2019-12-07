"""Day 5 - Intcode Asteroid."""


def make_code(buff, noun, verb):
    rbuff = buff.copy()
    rbuff[1] = noun
    rbuff[2] = verb
    return rbuff


def get_from_buff(buff, val, mode):
    if mode == 0:
        return buff[val]
    elif mode == 1:
        return val
    else:
        raise ValueError("Unexpected Parameter Mode: {0}".format(mode))


def take_step(buff, idx=0, input_buffer=None, output_buffer=None):
    op_code = buff[idx]
    instruction = op_code % 100
    p1_mode = (op_code % 1000) // 100
    p2_mode = (op_code % 10000) // 1000
    # p3_mode = (op_code % 100000) // 10000

    if instruction == 99:
        return buff, idx + 1, True, input_buffer, output_buffer
    elif instruction in (1, 2, 7, 8):
        arg1 = get_from_buff(buff, buff[idx + 1], p1_mode)
        arg2 = get_from_buff(buff, buff[idx + 2], p2_mode)
        if instruction == 1:
            # add
            res = arg1 + arg2
        elif instruction == 2:
            # multiply
            res = arg1 * arg2
        elif instruction == 7:
            res = int(arg1 < arg2)
        elif instruction == 8:
            res = int(arg1 == arg2)
        buff[buff[idx + 3]] = res
        idx += 4
    elif instruction == 3:
        # Get input
        if input_buffer is None:
            i = input('INPUT--> ')
        else:
            i = input_buffer.pop(0)
        i = int(i)
        buff[buff[idx + 1]] = i
        idx += 2
    elif instruction == 4:
        # Put Output
        arg1 = get_from_buff(buff, buff[idx + 1], p1_mode)
        if output_buffer is None:
            print('OUTPUT--> {0}'.format(arg1))
        else:
            output_buffer.append(arg1)
        idx += 2
    elif instruction in (5, 6):
        # Jump if true/false
        arg1 = get_from_buff(buff, buff[idx + 1], p1_mode)
        arg2 = get_from_buff(buff, buff[idx + 2], p2_mode)
        if (instruction == 5 and arg1 != 0) or (instruction == 6 and arg1 == 0):
            idx = arg2
        else:
            idx += 3
    else:
        raise ValueError("Unexpected Opcode: {0}".format(instruction))
    return buff, idx, False, input_buffer, output_buffer


def iter_codes(buff, input_buff=None):
    idx = 0
    stop = False
    output_buff = []
    while True:
        # print((idx, buff))
        if stop:
            return output_buff
        buff, idx, stop, input_buff, output_buff = take_step(
            buff, idx=idx, input_buffer=input_buff, output_buffer=output_buff)


amp_code = [3,8,1001,8,10,8,105,1,0,0,21,34,47,72,93,110,191,272,353,434,99999,3,9,102,3,9,9,1001,9,3,9,4,9,99,3,9,102,4,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,2,9,1002,9,2,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,101,5,9,9,102,4,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,102,4,9,9,1001,9,3,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99]


def amplifier(phase, in_signal):
    return iter_codes(amp_code, input_buff=[phase, in_signal])[0]

def try_comb(phaselist):
    sig = 0
    o = []
    for p in phaselist:
        sig = amplifier(p, sig)
    return sig

print(try_comb([0,1,2,3,4]))

def brute_force():
    best_val = -9999999
    best_comb = None
    for i1 in range(0,5):
        for i2 in range(0,5):
            for i3 in range(0,5):
                for i4 in range(0,5):
                    for i5 in range(0,5):
                        t = (i1, i2, i3, i4, i5)
                        # Check no repetitions
                        if len(t) == len(set(t)):
                            o = try_comb(t)
                            if o > best_val:
                                best_val = o
                                best_comb = t
    return best_comb, best_val

print(brute_force())