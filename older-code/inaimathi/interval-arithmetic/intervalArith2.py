class Interval:
    def __init__(self, center, radius):
        self.lower = center - radius
        self.upper = center + radius
        self.center = center
        self.radius = radius

    @classmethod
    def fromBounds(cls, lower, upper):
        rad = float(upper - lower) / 2
        return cls(lower + rad, rad)
    
    def add(self, b):
        return Interval.fromBounds(self.lower + b.lower, self.upper + b.upper)

    def subtract(self, b):
        return Interval.fromBounds(self.lower - b.upper, self.upper - b.lower)

    def multiply(self, b):
        (a, b, c, d) = (self.lower, self.upper, b.lower, b.upper)
        vals = [a * c, a * d, b * c, b * d]
        return Interval.fromBounds(min(vals), max(vals))

    def divide(self, b):
        if b.boundsZeroP():
            raise ZeroDivisionError, "You're dividing by an interval that bounds zero"
        (a, b, c, d) = (self.lower, self.upper, b.lower, b.upper)
        vals = [a / c, a / d, b / c, b / d]
        return Interval.fromBounds(min(vals), max(vals))

    def boundsZeroP(self):
        return (self.lower < 0) and (self.upper > 0)

    def test(self):
        res = ia.Interval(6, 4).add(ia.Interval(8, 5))
        assert(res.lower == 5.0)
        assert(res.upper == 23.0)
        assert(res.radius == 9.0)
        assert(res.center == 14.0)
