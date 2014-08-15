blinker = [[0,0,0,0,0],
           [0,0,1,0,0],
           [0,0,1,0,0],
           [0,0,1,0,0],
           [0,0,0,0,0]]

def isPositive(num):
    return num == abs(num)

def inBoard(board, x, y):
    return isPositive(x) and isPositive(y) and (x < len(board[0])) and (y < len(board))

def neighbours(board, x, y):
    rng = [-1, 0, 1]
    potentials = [(x + n, y + m) for n in rng for m in rng if (n, m) != (0, 0)]
    actuals = filter(lambda tup: inBoard(board, tup[0], tup[1]), potentials)
    return [board[n][m] for (n, m) in actuals]

def nextStep(board):
    """Takes a board, and returns its next generation."""
    res = []
    for i, row in enumerate(board):
        rowRes = []
        for j, col in enumerate(row):
            nCount = sum(neighbours(board, i, j))
            if nCount == 3:
                rowRes.append(1)
            elif nCount == 2 and col == 1:
                rowRes.append(1)
            else:
                rowRes.append(0)
        res.append(rowRes)
    return res

def runLife(board):
    b = board
    while True:
        for row in b:
            print row
        print "=========="
        b = nextStep(b)
