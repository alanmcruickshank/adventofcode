"""Advent of Code Day 25

https://adventofcode.com/2020/day/25

LOL. Public Key Encryption for Christmas?
"""

cases = [
    {'card': 5764801, 'door': 17807724},
    {'card': 16915772, 'door': 18447943}
]


def enc_loop(loop_size, subject_number=7, salt=20201227, init_val=1):
    val = init_val
    for _ in range(loop_size):
        val *= subject_number
        val %= salt
    return val


def rev_enc_loop(pub_key, subject_number=7, salt=20201227, init_val=1, limit=100000000):
    val = init_val
    for i in range(limit):
        val *= subject_number
        val %= salt
        if val == pub_key:
            loop_size = i + 1
            break
    else:
        raise RuntimeError("No Loop found up to limit.")
    return loop_size


for example in cases:
    card_key = example['card']
    door_key = example['door']
    print("Example. Card:", card_key, "Door:", door_key)
    card_loop = rev_enc_loop(card_key)
    door_loop = rev_enc_loop(door_key)
    print(card_loop, door_loop)
    card_shake = enc_loop(card_loop, subject_number=door_key)
    door_shake = enc_loop(door_loop, subject_number=card_key)
    print(card_shake, door_shake)
    # Part 1 answer: 6011069
