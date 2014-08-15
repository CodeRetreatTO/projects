from numpy import *
import unittest



class TestBrian(unittest.TestCase):
    def setUp(self):
        self.board = initialize_board(create_board(DIM, DIM))
    def testCycle(self):
        board = cycle(self.board)
        self.assertEqual(board[3,5], ALIVE)
        self.assertEqual(board[2,5], ALIVE)
        self.assertEqual(board[4,5], ALIVE)
        self.assertEqual(board[3,4], DYING)
        self.assertEqual(board[3,6], DYING)
        board = cycle(board)
        self.assertEqual(board[3,4], DEAD)
        self.assertEqual(board[3,6], DEAD)
        
        

DIM = 9
DEAD,DYING,ALIVE = range(3)

def create_board(x, y):
    board = zeros((x, y), dtype='i')
    return board
    
def initialize_board(board):
    board[3,4] = ALIVE
    board[3,6] = ALIVE
    return board

def cycle(in_board):
    out_board = array(in_board, copy=True)
    for row in range(in_board.shape[0]):
        for col in range(in_board.shape[1]):
            if in_board[row,col] == DYING:
                out_board[row,col] = DEAD
            elif in_board[row,col] == ALIVE:
                out_board[row,col] = DYING
            else:
                ## complex case
                ng = in_board[
                    max((row-1,0)):min((row+2,in_board.shape[0])), 
                    max((col-1,0)):min((col+2,in_board.shape[1]))
                ]
                if sum(ng == 2) >= 2:
                    #import pdb; pdb.set_trace()
                    out_board[row,col] = ALIVE
    return out_board

if __name__ == '__main__':
    board = initialize_board(create_board(DIM, DIM))
    print(board)
    for i in range(20):
        board = cycle(board)
        print(board)
        input("Next? ")
        

    
