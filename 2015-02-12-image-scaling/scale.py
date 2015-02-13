import math

pic = [[1, 2, 3],
       [4, 5, 6],
       [7, 8, 9]]

def scale (factor, canvas):
    h = int(math.ceil(len(canvas) * factor))
    w = int(math.ceil(len(canvas[0]) * factor))
    new_canvas = [[0 for c in xrange(w)] for ln in xrange(h)]
    for y in xrange(h):
        for x in xrange(w):
            new_x = int(x/factor)
            new_y = int(y/factor)
            new_canvas[y][x] = canvas[new_y][new_x]
    return new_canvas
    
