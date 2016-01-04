from util import Fraction, Polynomial as P, reduce, mul, add, prod


def interpolate(*points):
    """Takes in a list of tuples of x,y cordinates.

    >>> ipt = lambda y, d: interpolate(*[(x, y(x)) for x in range(d+1)])
    >>> ipt(lambda x: x**5 + 3*x**3 + x, 5)
    x^5 + 3x^3 + x
    >>> ipt(lambda x: 7*x**4 + 5*x**3 + 9*x**2 + 3, 4)
    7x^4 + 5x^3 + 9x^2 + 3
    """
    points = set(points)
    return reduce(add, [
        Fraction(
            prod(P((1, 1), (-x2, 0)) for x2, y2 in points if (x,y) != (x2,y2)),
            prod(P((x,0), (-x2, 0)) for x2, y2 in points if (x,y) != (x2,y2))
        ) * y for x, y in points])
