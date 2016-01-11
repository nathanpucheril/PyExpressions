from Expressions import *

def polynomialGCD(x, y):
    x, y = map(Polynomial.polify, (x, y))
    if y.degree > x.degree:
        return polynomialGCD(y, x)
    if y == 0:
        return x
    return polynomialGCD(y, x % y)
