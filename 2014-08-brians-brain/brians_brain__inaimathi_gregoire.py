### Completely not working. Sorry about that.
### Unfamiliar approach combined with style conflicts do that on a 45min timeline

class Grid():
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.cells=[Cell(x, y) for x in xrange(width) for y in xrange(height)]
        for c in self.cells:
            tmp = [((c.x+mod_x) + (self.width*(c.y+mod_y))) for mod_x in [-1, 0, 1] for mod_y in [-1, 0, 1] if ((0, 0) != (mod_x, mod_y))]
            c.neighbors = [i for i in tmp if ((width*height) > i >= 0)]

    def poke(self, x, y, new_state):
        self.cells[x+(self.width*y)].state = new_state

    def show(self):
        for (i, c) in enumerate(self.cells):
            c.show()
            if i % self.width == (self.width-1):
                print ""

    def glider(self):
        self.poke(2, 1, "dying")
        self.poke(2, 2, "on")
        self.poke(3, 2, "on")
        self.poke(4, 2, "dying")
        self.poke(1, 3, "dying")
        self.poke(2, 3, "on")
        self.poke(3, 3, "on")
        self.poke(3, 4, "dying")

    def next(self):
        for c in self.cells:
            c.next(self.cells)
        
        

class Cell():
    def __init__(self, x, y, state="off"):
        self.x = x
        self.y = y
        self.state = state
        self.neighbors = []

    def show(self):
        if self.state == "on":
            print 1,
        elif self.state == "dying":
            print 2,
        elif self.state == "off":
            print 0,

    def live_neighbors(self, cells):
        res = 0
        for i in self.neighbors:
            if cells[i].state == "on":
                res = res +1
        return res
            
    def next(self, cells):
        if self.state == "off" and self.live_neighbors(cells)==2:
            self.state = "on"
        elif self.state == "on":
            self.state = "dying"
        elif self.state == "dying":
            self.state = "off"
