#!/usr/bin/python
"""
Transliteration of Maggesi's tiny haskell sudoku solver: 
  http://web.math.unifi.it/users/maggesi/haskell_sudoku_solver.html

Not the ideal way to solve sudoku, but its small and elegant.

A board is represented by a function that maps coordinates to
possible values.  

The mark function returns a new board with a particular coordinate 
filled in and conflicts removed from related rows, columns and lines.

The solve function goes through and makes new possible boards 
by making guesses from the set of possibilities for each square.

All solutions are collected and printed.

Note: reductions are backwards because order wasn't important
and python comes with a left fold function but not a right fold.
"""

import sys

rs = range(1,10)
idx = [(i,j) for i in rs for j in rs]

def input(s) :
    def allVals(p) :
        return rs
    return reduce(mark, ((p,n) for p,n in zip(idx, (int(w) for l in s.split('\n') for w in l.split(' '))) if n > 0), allVals)

def disp(s) :
    return '\n'.join(' '.join(str(s((i,j))[0]) for j in rs) for i in rs)

def e(a, b) :
    return (a - 1) / 3 == (b - 1) / 3

def mark(s, pn) :
    p,n = pn
    i,j = p
    def filtered(q) :
        x,y = q
        if p == q :
            return [n]
        elif x == i or y == j or (e(x, i) and e(y, j)) :
            return [x for x in s(q) if x != n]
        else :
            return s(q)
    return filtered

def solve(s) :
    def search(l, p) :
        return [mark(s2, (p, n)) for s2 in l for n in s2(p)]
    return reduce(search, idx, s)

def main() :
    for fn in sys.argv[1:] :
        s = file(fn).read()[:-1]
        print '\n'.join(map(disp, solve([input(s)])))

if __name__ == '__main__' :
    main()
