import time
DEAD,DYING,ALIVE=range(3)

def main():
    width,height = 30,30
    offsets = [[-1,-1],[-1,0],[-1,1],[0,1],[1,1],[1,0],[1,-1],[0,-1]]
    board = [[0]*height for w in range(width)]
    
    initial = [[3,4],[3,6],[5,8],[6,9]]
    
    for (x,y) in initial:
        board[x][y] = ALIVE
    
    def neighbours( x,y ):
        for (dx,dy) in offsets:
            yield board[(x+dx)%width][(y+dy)%height]
    
    def rule( x, y, board ):
        value = board[x][y]
        if value == DYING:
            return DEAD 
        elif value == ALIVE:
            return DYING
        else:
            count = 0
            for neighbour in neighbours( x,y ):
                if neighbour == ALIVE:
                    count += 1
                    if count > 2:
                        break
            if count == 2:
                return ALIVE 
            return DEAD
    
    def cycle( board, rule ):
        return [
            [rule(x,y,board) for y in range(height)]
            for x in range(width)
        ]
    def representation( board ):
        return '\n'.join([
            ''.join([
                {ALIVE:'#',DEAD:' ',DYING:'-'}[c]
                for c in row
            ])
            for row in board 
        ])
        
    
    while True:
        board = cycle( board, rule )
        print representation( board )
        print 
        time.sleep( .1 )

if __name__ == "__main__":
    main()
    
