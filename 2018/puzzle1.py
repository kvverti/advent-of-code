# Lavender takes far too long on Puzzle 1, part 2 without the
# ability to efficiently build dictionaries.

import sys

def parseMods(input):
    return input.split("\n")

def findTargetFreq(mods):
    seen = set()
    freq = 0
    idx = 0
    while freq not in seen:
        seen |= { freq }
        freq += int(mods[idx])
        idx = (idx + 1) % len(mods)
    return freq

if __name__ == "__main__":
    mods = parseMods(sys.argv[1])
    print(findTargetFreq(mods))
