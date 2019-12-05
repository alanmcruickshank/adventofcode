"""Day 2 - Intcode."""

test_code = [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]

puzzle_code = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,5,19,23,2,9,23,27,1,27,5,31,2,31,13,35,1,35,9,39,1,39,10,43,2,43,9,47,1,47,5,51,2,13,51,55,1,9,55,59,1,5,59,63,2,6,63,67,1,5,67,71,1,6,71,75,2,9,75,79,1,79,13,83,1,83,13,87,1,87,5,91,1,6,91,95,2,95,13,99,2,13,99,103,1,5,103,107,1,107,10,111,1,111,13,115,1,10,115,119,1,9,119,123,2,6,123,127,1,5,127,131,2,6,131,135,1,135,2,139,1,139,9,0,99,2,14,0,0]
# Make sure we make the specified adjustments
puzzle_code = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,5,19,23,2,9,23,27,1,27,5,31,2,31,13,35,1,35,9,39,1,39,10,43,2,43,9,47,1,47,5,51,2,13,51,55,1,9,55,59,1,5,59,63,2,6,63,67,1,5,67,71,1,6,71,75,2,9,75,79,1,79,13,83,1,83,13,87,1,87,5,91,1,6,91,95,2,95,13,99,2,13,99,103,1,5,103,107,1,107,10,111,1,111,13,115,1,10,115,119,1,9,119,123,2,6,123,127,1,5,127,131,2,6,131,135,1,135,2,139,1,139,9,0,99,2,14,0,0]


example_code = [1,1,1,4,99,5,6,0,99]

def take_step(buff, idx=0):
    op_code = buff[idx]

    if op_code == 99:
        return buff, None, True
    elif op_code == 1:
        # add
        arg1 = buff[buff[idx + 1]]
        arg2 = buff[buff[idx + 2]]
        buff[buff[idx + 3]] = arg1 + arg2
    elif op_code == 2:
        # multiply
        arg1 = buff[buff[idx + 1]]
        arg2 = buff[buff[idx + 2]]
        buff[buff[idx + 3]] = arg1 * arg2
    else:
        raise ValueError("Unexpected Opcode: {0}".format(op_code))
    return buff, idx + 4, False


def iter_codes(buff):
    idx = 0
    stop = False
    while True:
        print((idx, buff))
        if stop:
            return buff
        buff, idx, stop = take_step(buff, idx=idx)


iter_codes(puzzle_code)
