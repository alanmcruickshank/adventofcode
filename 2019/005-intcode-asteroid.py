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


def take_step(buff, idx=0):
    op_code = buff[idx]
    instruction = op_code % 100
    p1_mode = (op_code % 1000) // 100
    p2_mode = (op_code % 10000) // 1000
    # p3_mode = (op_code % 100000) // 10000

    if instruction == 99:
        return buff, idx + 1, True
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
        i = input('INPUT--> ')
        i = int(i)
        buff[buff[idx + 1]] = i
        idx += 2
    elif instruction == 4:
        # Put Output
        arg1 = get_from_buff(buff, buff[idx + 1], p1_mode)
        print('OUTPUT--> {0}'.format(arg1))
        idx += 2
    elif instruction in (5, 6):
        # Jump if true/false
        arg1 = get_from_buff(buff, buff[idx + 1], p1_mode)
        arg2 = get_from_buff(buff, buff[idx + 2], p2_mode)
        if (instruction == 5 and arg1 != 0) or (instruction == 6 and arg1 == 0):  # noqa
            idx = arg2
        else:
            idx += 3
    else:
        raise ValueError("Unexpected Opcode: {0}".format(instruction))
    return buff, idx, False


def iter_codes(buff):
    idx = 0
    stop = False
    while True:
        # print((idx, buff))
        if stop:
            return buff
        buff, idx, stop = take_step(buff, idx=idx)


r = iter_codes([3,225,1,225,6,6,1100,1,238,225,104,0,1101,48,82,225,102,59,84,224,1001,224,-944,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,1101,92,58,224,101,-150,224,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1102,10,89,224,101,-890,224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,1101,29,16,225,101,23,110,224,1001,224,-95,224,4,224,102,8,223,223,1001,224,3,224,1,223,224,223,1102,75,72,225,1102,51,8,225,1102,26,16,225,1102,8,49,225,1001,122,64,224,1001,224,-113,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1102,55,72,225,1002,174,28,224,101,-896,224,224,4,224,1002,223,8,223,101,4,224,224,1,224,223,223,1102,57,32,225,2,113,117,224,101,-1326,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1,148,13,224,101,-120,224,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,677,226,224,102,2,223,223,1006,224,329,101,1,223,223,107,677,677,224,1002,223,2,223,1006,224,344,101,1,223,223,8,226,677,224,102,2,223,223,1006,224,359,101,1,223,223,107,226,226,224,102,2,223,223,1005,224,374,1001,223,1,223,1108,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,107,677,226,224,102,2,223,223,1006,224,404,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,419,1001,223,1,223,108,677,677,224,102,2,223,223,1005,224,434,1001,223,1,223,1008,677,226,224,1002,223,2,223,1006,224,449,1001,223,1,223,7,226,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1007,677,677,224,102,2,223,223,1005,224,479,1001,223,1,223,1007,226,226,224,1002,223,2,223,1005,224,494,1001,223,1,223,108,226,226,224,1002,223,2,223,1005,224,509,1001,223,1,223,1007,226,677,224,1002,223,2,223,1006,224,524,101,1,223,223,1107,677,677,224,102,2,223,223,1005,224,539,101,1,223,223,1107,677,226,224,102,2,223,223,1005,224,554,1001,223,1,223,108,677,226,224,1002,223,2,223,1006,224,569,1001,223,1,223,1108,226,677,224,1002,223,2,223,1006,224,584,101,1,223,223,8,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,614,101,1,223,223,7,677,677,224,1002,223,2,223,1006,224,629,101,1,223,223,1008,677,677,224,102,2,223,223,1005,224,644,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,659,101,1,223,223,1108,226,226,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226])  # noqa
