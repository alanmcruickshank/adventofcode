"""Advent of Code Day 7

https://adventofcode.com/2020/day/7

Haversacks. Inverting BINARY TREES. LOL.
"""

def load_rules_file(fname):
    """Load the rules file and make an initial rule tree.

    Returns:
        dict of dicts, saying which bags must contain other bags:
            e.g {'bag': {'bag_a': 1}}
    """
    rules_def = {}
    with open(fname) as txt_file:
        for line in txt_file:
            line = line.strip()
            root_bag, contains_bags = line.split('contain')
            root_bag = ' '.join(root_bag.split(' ')[:2])

            contains_bags = contains_bags.split(',')
            content = {}
            for elem in contains_bags:
                elem = elem.strip(" .")
                # Skip the things that say nothing else.
                if elem == 'no other bags':
                    continue
                bag_count = int(elem.split(' ')[0])
                bag_type = ' '.join(elem.split(' ')[1:3])
                content[bag_type] = bag_count
            rules_def[root_bag] = content
    return rules_def


def iter_rule_pairs(rules_def):
    for root_bag in rules_def:
        for contained_bag in rules_def[root_bag]:
            yield root_bag, contained_bag


def invert_rules(rules_def):
    """Invert the rules definition to give the contained rules."""
    contained_rules = {}
    for root_bag, contained_bag in iter_rule_pairs(rules_def):
        if contained_bag in contained_rules:
            contained_rules[contained_bag].append(root_bag)
        else:
            contained_rules[contained_bag] = [root_bag]
    return contained_rules


def containing_bag_options(contained_rules, search_bag):
    # Make an initial set of options.
    options = set()
    option_buffer = set()
    # Deal with the case of no options
    if search_bag not in contained_rules:
        return options

    for containing_bag in contained_rules[search_bag]:
        option_buffer.add(containing_bag)
    
    # Try to expand the list. Can we add any more bags to our set?
    while option_buffer:
        new_option_buff = set()
        for option in option_buffer:
            if option in contained_rules:
                for new_option in contained_rules[option]:
                    if new_option not in options and new_option not in option_buffer:
                        new_option_buff.add(new_option)
            options.add(option)
        option_buffer = new_option_buff
    return options


def count_containing_bags(rules_def, search_bag):
    # Always count it self if nothing ele
    running_total = 1
    # Add on child bags if they exist [i.e. RECURSE].
    if search_bag in rules_def:
        for child_bag in rules_def[search_bag]:
            per_child = count_containing_bags(rules_def, child_bag)
            num_children = rules_def[search_bag][child_bag]
            running_total += num_children * per_child
    return running_total


for file in ["007-haversacks-test.txt", "007-haversacks-test2.txt", "007-haversacks-input.txt"]:
    rules_def = load_rules_file(file)
    # Part 1
    contained_rules = invert_rules(rules_def)
    options = containing_bag_options(contained_rules, 'shiny gold')
    # Part 2. (Minus one so we don't count the gold bag)
    contained_bags = count_containing_bags(rules_def, 'shiny gold') - 1
    print(file, len(options), contained_bags)
    
