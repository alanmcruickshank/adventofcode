"""Advent of Code Day 22

https://adventofcode.com/2020/day/22

Crabs
"""


def calc_scores(deck):
    return sum((idx + 1) * elem for idx, elem in enumerate(reversed(deck)))


def play_regular_combat(p1_deck, p2_deck, recurse=False):
    p1_deck = p1_deck.copy()
    p2_deck = p2_deck.copy()
    previous_game_positions = set()
    previous_game_positions.add(tuple([tuple(p1_deck), tuple(p2_deck)]))
    while p1_deck and p2_deck:
        # Pop off the cards.
        p1_card, p2_card = p1_deck.pop(0), p2_deck.pop(0)
        # if recurse AND enough cards, recurse. Otherwise normal.
        if recurse and len(p1_deck) >= p1_card and len(p2_deck) >= p2_card:
            # Recursive game!
            winner, _ = play_regular_combat(p1_deck[:p1_card], p2_deck[:p2_card], recurse=True)
        else:
            if p1_card > p2_card:
                winner = 1
            elif p2_card > p1_card:
                winner = 2
            else:
                raise ValueError("Huh!?")

        if winner == 1:
            p1_deck += [p1_card, p2_card]
        elif winner == 2:
            p2_deck += [p2_card, p1_card]
        else:
            raise ValueError("Wat!?")

        game_pos = tuple([tuple(p1_deck), tuple(p2_deck)])
        if game_pos in previous_game_positions:
            # Win in favour of p1
            p2_deck = []
            break
        previous_game_positions.add(game_pos)
    # Check winner and calculate score.
    if p1_deck:
        score = calc_scores(p1_deck)
        winner = 1
    elif p2_deck:
        score = calc_scores(p2_deck)
        winner = 2
    return winner, score


for fname in ['022-3.txt', '022-1.txt', '022-2.txt']:
    print(fname)
    with open(fname) as f:
        p1, _, p2 = f.read().partition('\n\n')
    # Set up the decks
    p1_deck = [int(elem) for elem in p1.strip().splitlines(keepends=False)[1:]]
    p2_deck = [int(elem) for elem in p2.strip().splitlines(keepends=False)[1:]]
    # Play the game
    winner, score = play_regular_combat(p1_deck, p2_deck, recurse=False)
    print("No Recurse Winner", winner, "Score", score)
    # Part 1 answer: 33393
    winner, score = play_regular_combat(p1_deck, p2_deck, recurse=True)
    print("Recurse Winner", winner, "Score", score)
