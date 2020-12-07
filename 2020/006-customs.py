"""Advent of Code Day 6

https://adventofcode.com/2020/day/6

Read and buffer, just as before.
"""

def read_customs_answers(fname, part=1):
    groups = []
    response_buff = []
    with open(fname) as txt_file:
        for line in txt_file:
            resps = set(line.strip(' \n'))
            if resps:
                response_buff.append(resps)
            else:
                if response_buff:
                    if part == 1:
                        response_buff = response_buff[0].union(*response_buff[1:])
                    elif part == 2:
                        response_buff = response_buff[0].intersection(*response_buff[1:])
                    groups.append(response_buff)
                response_buff = []
    # Handle any trailing groups
    if response_buff:
        if part == 1:
            response_buff = response_buff[0].union(*response_buff[1:])
        elif part == 2:
            response_buff = response_buff[0].intersection(*response_buff[1:])
        groups.append(response_buff)
    return groups

for file in ["006-customs-test.txt", "006-customs-input.txt"]:
    for part in [1, 2]:
        answers = read_customs_answers(file, part=part)
        num_responses = [len(elem) for elem in answers]
        sum_responses = sum(num_responses)
        print(file, "part", part, sum_responses)
