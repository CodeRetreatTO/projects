def makeBoard(regionWidth):
    side = regionWidth * regionWidth
    return [[0 for x in xrange(side)] for y in xrange(side)]

sample = [[0, 7, 1, 4, 0, 0, 0, 0, 5],
          [0, 0, 0, 0, 5, 0, 0, 8, 0],
          [0, 0, 3, 9, 0, 7, 6, 0, 0],
          [0, 0, 0, 0, 0, 1, 0, 0, 0],
          [0, 9, 0, 8, 0, 6, 0, 0, 3],
          [0, 0, 0, 0, 0, 0, 8, 2, 0],
          [0, 6, 0, 0, 4, 0, 7, 0, 8],
          [3, 0, 0, 0, 0, 0, 0, 9, 0],
          [0, 0, 0, 0, 8, 5, 0, 0, 0]]

def placements(board, n):
    """Return list of coords where `n` is a legal placement."""
    emptyRows = [i for i, row in enumerate(board) if not n in row]
    res = []
    for i in emptyRows:
        for j, col in enumerate(board[i]):
            if col == 0: res.append((i, j))
    return res
