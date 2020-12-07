"""Advent of Code Day 6

https://adventofcode.com/2020/day/6

Read and buffer, just as before.
"""

def compress_response_buff(buff, part=1):
    if buff:
        if part == 1:
            return buff[0].union(*buff[1:])
        elif part == 2:
            return buff[0].intersection(*buff[1:])
    return set()


def read_customs_answers(fname, part=1):
    groups = []
    response_buff = []
    with open(fname) as txt_file:
        for line in txt_file:
            resps = set(line.strip(' \n'))
            if resps:
                response_buff.append(resps)
            else:
                groups.append(compress_response_buff(response_buff, part=part))
                response_buff = []
    # Handle any trailing groups
    groups.append(compress_response_buff(response_buff, part=part))
    return groups

for file in ["006-customs-test.txt", "006-customs-input.txt"]:
    for part in [1, 2]:
        answers = read_customs_answers(file, part=part)
        num_responses = [len(elem) for elem in answers]
        sum_responses = sum(num_responses)
        print(file, "part", part, sum_responses)
