import re
import sys

# Helper class that stores properties of a claim.
class Claim:
    def __init__(self, nums):
        self.id = int(nums[0])
        self.x1 = int(nums[1])
        self.y1 = int(nums[2])
        self.x2 = int(nums[1]) + int(nums[3])
        self.y2 = int(nums[2]) + int(nums[4])

# Parses the string input into a list of Claims.
def parseInput(input):
    lines = input.strip().split("\n")
    grid = []
    for line in lines:
        nums = re.split("[#@,:x\s]+", line)
        grid.append(Claim(nums[1:]))
    return grid

# Computes the overlapping regions in the given claims.
# The grid returned is a dict of the form:
# (x, y): [ id1, id2, .... idn ]
# where (x, y) are the coordinates and id1..n are the ids
# that cover this space.
def computeClaims(claims):
    res = {}
    for claim in claims:
        for xi in range(claim.x1, claim.x2):
            for yi in range(claim.y1, claim.y2):
                key = xi, yi
                if key not in res:
                    res[key] = [claim.id]
                else:
                    res[key] += [claim.id]
    return res

# Returns whether two claims are disjoint, that is, they share no
# spaces in common.
def disjoint(claim1, claim2):
    return ((claim1.x1 >= claim2.x2 or claim1.x2 <= claim2.x1)
        or (claim1.y1 >= claim2.y2 or claim1.y2 <= claim2.y1))

# Filters a list of claims so that only claims disjoint to every other
# claim remain.
def filterDisjointClaim(claims):
    res = list(claims)
    for k in range(len(claims)):
        for m in range(k + 1, len(claims)):
            if not disjoint(claims[k], claims[m]):
                res[k] = None
                res[m] = None
    return list(filter(lambda a: a is not None, res))

if __name__ == "__main__":
    mode = int(sys.argv[1])
    input = parseInput(sys.argv[2])
    res = computeClaims(input)
    if mode == 1:
        for k in list(res.keys()):
            if len(res[k]) == 1:
                del res[k]
        print(len(res))
    elif mode == 2:
        claim = filterDisjointClaim(input)
        print(claim[0].id)
