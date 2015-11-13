import math

## We're so sorry.

test_shapes = {
    "square": [(0, 10), (10, 10), (10, 0), (0, 0)],
    "triangle": [(0, 10), (10, 5), (0, 0)]
}

def rotatePolygon(polygon,theta):
    """Rotates the given polygon which consists of corners represented as (x,y),
    around the ORIGIN, clock-wise, theta degrees.

    ATTRIBUTION!: http://stackoverflow.com/a/20024348/190887"""
    theta = math.radians(theta)
    rotatedPolygon = []
    for corner in polygon :
        rotatedPolygon.append(( corner[0]*math.cos(theta)-corner[1]*math.sin(theta) , corner[0]*math.sin(theta)+corner[1]*math.cos(theta)) )
    return rotatedPolygon

def _naive_width(shape):
    [xs, ys] = zip(*shape)
    return max(xs) - min(xs)

def min_projection_from_shape(shape):
    return min([_naive_width(rotatePolygon(shape, i)) for i in xrange(1, 361)])

def has_hole_at_least(width):
    return False

def main():
    print "Blargh!"

if __name__ == '__main__':
    main()
