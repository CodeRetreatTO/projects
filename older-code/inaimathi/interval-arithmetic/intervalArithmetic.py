def add(a, b):
    return (a[0] + b[0], a[1] + b[1])

def sub(a, b):
    return (a[0] - b[1], a[1] - b[0])

def multiply(a, b):
    ((l, u), (l2, u2)) = (a, b)
    vals = [l * l2, l * u2, u * l2, u * u2]
    return (min(vals), max(vals))

def divide(a, b):
    ((l, u), (l2, u2)) = (a, b)
    if ((l2 < 0 and u2 > 0) or (u2 < 0 and l2 > 0)):
        raise ZeroDivisionError, "You tried to divide by an interval that bounds 0"
    vals = [l / l2, l / u2, u / l2, u / u2]
    return (min(vals), max(vals))
