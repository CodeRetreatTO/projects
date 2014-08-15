# Note: the main function was not finised during the retreat,
# and the indexing/lookup was done in such a way that we were not 
# wrapping at the edges
from numpy import *
import unittest, time

class TestBrian(unittest.TestCase):
    def setUp(self):
        self.board = initialize_board(
            create_board(*DIM),
            [[3,4],[3,6]],
        )
    def testCycle(self):
        board = self.board
        self.assertEqual(board[3,4], ALIVE)
        self.assertEqual(board[3,6], ALIVE)
    
        board = cycle(self.board)
        self.assertEqual(board[3,5], ALIVE)
        self.assertEqual(board[2,5], ALIVE)
        self.assertEqual(board[4,5], ALIVE)
        self.assertEqual(board[3,4], DYING)
        self.assertEqual(board[3,6], DYING)
        board = cycle(board)
        self.assertEqual(board[3,4], DEAD)
        self.assertEqual(board[3,6], DEAD)

DIM = 50,50
DEAD,DYING,ALIVE = 0x000000ff, 0xff0000ff, 0xffffffff
OFFSETS = array([[-1,-1],[-1,0],[-1,1],[0,1],[1,1],[1,0],[1,-1],[0,-1]],dtype='b')

def create_board(x, y):
    board = zeros((x, y), dtype='I')
    return board
    
def initialize_board(board, start_position):
    for (x,y) in start_position:
        board[x,y] = ALIVE
    return board

def cycle(in_board):
    out_board = zeros(in_board.shape, dtype='I')
    for x in range(in_board.shape[0]):
        for y in range(in_board.shape[1]):
            if in_board[x,y] == DYING:
                out_board[x,y] = DEAD
            elif in_board[x,y] == ALIVE:
                out_board[x,y] = DYING
            else:
                ## complex case
                # Simple pythonic implementation
#                count = 0
#                for dx,dy in OFFSETS:
#                    if in_board[(x+dx)%in_board.shape[0],(y+dy)%in_board.shape[1]] == ALIVE:
#                        count += 1
#                        if count > 2:
#                            break
#                if count == 2:
#                    out_board[x,y] = ALIVE
                # numpy implementation...
                nindices = (OFFSETS+(x,y))%in_board.shape 
                values = in_board[nindices[:,0],nindices[:,1]]
                if sum(values==ALIVE) == 2:
                    out_board[x,y] = ALIVE
    return out_board

def main():
    import pygame
    from pygame import surfarray,transform
    screen_size = (DIM[0]*4,DIM[1]*4)
    screen = pygame.display.set_mode(screen_size)
    pygame.display.init()
    pygame.init()
    board = create_board(*DIM)
    board = initialize_board(
        board,
        [[5,5],[5,6]],
    )
    rate = .25
    next = time.time() -1
    small = pygame.Surface( DIM )
    blitter = pygame.Surface( screen_size )
    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                raise SystemExit(0)
        if time.time() > next:
            next = time.time() + rate 
            board = cycle(board)
            surfarray.blit_array(small,board)
            transform.scale( small, screen_size, blitter )
        screen.blit( blitter, (0,0) )
        #surfarray.blit_array(screen,board)
        pygame.display.flip()
    
if __name__ == '__main__':
    main()
