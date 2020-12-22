"""Advent of Code Day 22

https://adventofcode.com/2020/day/22

Crabs
"""


def calc_scores(deck):
    return sum((idx + 1) * elem for idx, elem in enumerate(reversed(deck)))


for fname in ['022-1.txt', '022-2.txt']:
    print(fname)
    with open(fname) as f:
        p1, _, p2 = f.read().partition('\n\n')
    # Set up the decks
    p1_deck = [int(elem) for elem in p1.strip().splitlines(keepends=False)[1:]]
    p2_deck = [int(elem) for elem in p2.strip().splitlines(keepends=False)[1:]]
    # Play the game
    while p1_deck and p2_deck:
        # Pop off the cards.
        p1_card, p2_card = p1_deck.pop(0), p2_deck.pop(0)
        if p1_card > p2_card:
            p1_deck += [p1_card, p2_card]
        elif p2_card > p1_card:
            p2_deck += [p2_card, p1_card]
        else:
            ValueError("Huh!?")
    # Check winner and calculate score.
    if p1_deck:
        print("P1 wins.")
        score = calc_scores(p1_deck)
    elif p2_deck:
        print("P2 wins.")
        score = calc_scores(p2_deck)
    print("Score:", score)
