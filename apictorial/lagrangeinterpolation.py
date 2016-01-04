from util import Fraction, Polynomial as P, reduce, mul, add, prod


def interpolate(*points):
    """Takes in tuples of x,y cordinates and returns a polynomial object.

    >>> coords = lambda y: [(x, y(x)) for x in range(y.degree+1)]
    >>> interpolate(*coords(P('x^5 + 3x^3 + x')))
    x^5 + 3x^3 + x
    >>> interpolate(*coords(P('7x^4 + 5x^3 + 9x^2 + 3')))
    7x^4 + 5x^3 + 9x^2 + 3
    """
    points = set(points)
    return reduce(add, [
        Fraction(
            prod(P((1, 1), (-x2, 0)) for x2, y2 in points if (x,y) != (x2,y2)),
            prod(P((x,0), (-x2, 0)) for x2, y2 in points if (x,y) != (x2,y2))
        ) * y for x, y in points])
