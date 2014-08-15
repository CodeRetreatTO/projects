from itertools import tee, izip

aromaticTable = {"I":1, "V":5, "X":10, "L":50, "C":100, "D":500, "M":1000}

def pairwise(iterable):
    a = iter(iterable)
    return izip(a, a)

def eachCons(iterable):
    a, b = tee(iterable)
    next(b, None)
    return izip(a, b)

def fromAromatic(aString):
    pairs = [(int(a), aromaticTable[r]) for a, r in pairwise(aString)]
    print pairs
    last = pairs[-1]
    print last
    res = 0
    for p, p2 in eachCons(pairs):
        if p2 and p2[1] > p[1]:
            res -= p[0]*p[1]
        else:
            res += p[0]*p[1]
    return res
