import numbers
from operator import mul, add
from functools import reduce

# multiplicative compliment to sum
prod = lambda l: reduce(mul, l)

# if a fraction or number
isNumeric = lambda n: isinstance(n, (Fraction, numbers.Number))

# coerce to a polynomial
poly = lambda p: P.make_constant(p) if isNumeric(p) else p

# converts to int if float value is equal to int value
pint = lambda n: int(n) if int(n) == float(n) else n


class Polynomial(object):

    def __init__(self, terms=[], *_terms, var = "x"):
        """
        Polynomial can be called with a single list of multiple points, or
        with multiple arguments, where each argument is a point.

        >>> P((3, 2), (2, 0))
        3x^2 + 2
        >>> P([(3, 2), (2, 0)])
        3x^2 + 2
        >>> P('3x^2 + 2')
        3x^2 + 2
        """
        assert len(terms) or len(_terms), 'No terms provided.'
        assert isinstance(var, str), "Variable must be a string."
        if isinstance(terms, str):
            terms = Polynomial.parse_terms(terms, var)
        __terms = terms if hasattr(terms[0], '__iter__') else (terms,) + _terms
        assert all([isinstance(t, tuple) and len(t) == 2 for t in __terms]), \
            "Terms must be tuples of the form (coefficient, exponent)."
        self.termsList = self.unsimplified_terms = __terms
        self.combine_terms()
        self.sort()
        self.var = var

    @staticmethod
    def parse_terms(string, var='x'):
        """Converts string of terms into list of (coefficient, exponent) tuples.

        >>> P.parse_terms('x^3 + 3x^2 + 3x + 2')
        [(1, 3), (3, 2), (3, 1), (2, 0)]
        """
        terms = []
        for term in filter(lambda x: x not in ('-', '+'), string.split(' ')):
            if var not in term:
                terms.append((term, 0))
            elif var == term:
                terms.append((1, 1))
            elif '^' not in term:
                term = term.split('x')
                terms.append((term[0], 1))
            elif term[0] == var:
                terms.append((1, term[2:]))
            else:
                terms.append(term.split('%s^' % var))
        coerce = lambda n: pint(float(n))
        return [(coerce(coeff), coerce(exp)) for coeff, exp in terms]

    @property
    def terms(self):
        return self.termsList

    def copy(self):
        return Polynomial(self.terms,self.var)

    def sort(self):
        self.termsList = sorted(self.terms, key = lambda x: x[1],reverse = True)
        return self

    @staticmethod
    def make_constant(c):
        """creates a Polynomial representation of a constant"""
        assert isNumeric(c), 'Non-numerics cannot be coerced to a constant.'
        return Polynomial((c, 0))

    def is_constant(self):
        """tests if polynomial is a constant"""
        return len(self.terms) == 1 and self.terms[0][1] == 0

    def combine_terms(self):
        """Combine terms, by summing all coefficients of the same degree

        >>> P('3x^3 + 3x^5 + 2x^5').combine_terms()
        3x^3 + 5x^5
        """
        combined = {}
        for coeff, exp in self.terms:
            combined.setdefault(exp, []).append(coeff)
        self.termsList = [(sum(v), k) for k, v in combined.items()]
        return self

    def clean(self):
        """Removes all terms with coefficient 0

        >>> P('3x^5 + 0x^4 + 3x').clean()
        3x^5 + 3x
        """
        self.termsList = list(filter(lambda term: term[0], self.termsList))
        return self

    def __add__(p1, p2):
        """Implements addition for Polynomial object

        >>> P((1, 0)) + P((2, 1), (2, 0))
        2x + 3
        >>> P((3, 0), (4, 3)) + 4
        4x^3 + 7
        """
        return Polynomial(poly(p1).terms + poly(p2).terms)

    def __sub__(p1, p2):
        """Implements subtraction for Polynomial object

        >>> P('1') - P('2x + 2')
        -2x - 1
        >>> P('3 + 4x^3') - 4
        4x^3 - 1
        """
        return p1 + -p2

    def __neg__(p):
        """Implements negation for Polynomial object

        >>> -P('x^4')
        -1x^4
        """
        return p * -1

    def __mul__(p1, p2):
        """Implements multiplication for Polynomial object. Note: Multiplication
        by non-Polynomial objects, like integers or floats, can only be
        accomplished in one format: Polynomial * non-Polynomial

        >>> P('1') * -5
        -5
        >>> P('3x^2 + x') * P('x^3 + 3x^2')
        3x^5 + 10x^4 + 3x^3
        """
        multiply = lambda p1, p2: (p1[0] * p2[0], p1[1] + p2[1])
        return Polynomial(sum([[multiply(p1, p2) for p1 in poly(p1).terms]
            for p2 in poly(p2).terms], []))

    def __xor__(self, n):
        """Implements xor as exponentiation

        >>> x = P('x^0')
        >>> (x^4)*3
        3x^4
        >>> p = (x^5)*3 + x*2 + 3
        >>> p(1)
        8
        """
        return Polynomial([(c, e+n) for c, e in self.terms])

    def __div__(p1, p2):
        """Implements division for Polynomial object"""
        raise NotImplemented()

    def __float__(p1):
        """Implements coercion to floating point for Polynomial objects"""
        if Polynomial.is_constant(p1):
            return float(p1.terms[0][0])
        raise TypeError('Non-constant polynomial cannot be coerced to float.')

    def __repr__(self):
        return str(self)

    def __str__(self):
        """Stringifies polynomials

        >>> P('3x^2')
        3x^2
        >>> P('3x') # no caret (^), if exponent is 1
        3x
        >>> P('3') # no variable if exponent is 0
        3
        >>> P('x') # no coefficient if coefficient is 1
        x
        >>> P('0x^2 + x') # no terms with coefficient 0
        x
        """
        def stringify(term):
            coefficient, exponent = term
            exp = {
                0: '',
                1: self.var
            }.get(exponent, '%s^%d' % (self.var, exponent))
            cof = '' if coefficient == 1 else str(pint(coefficient))
            return '%s%s' % (cof, exp)
        string = ''
        for t in map(stringify, filter(lambda t: t[0], self.terms)):
            if not string:
                string += t
            elif t[0] == '-':
                string += ' - %s' % t[1:]
            else:
                string += ' + %s' % t
        return string

    #################
    # FUNCTIONALITY #
    #################

    def asFunction(self):
        """Returns lambda function representing this polynomial.

        >>> p = P('x^2 + 3x^6 + 2x + 3').asFunction()
        >>> p(0)
        3
        >>> p(1)
        9
        """
        return lambda x: sum([cof*(x**exp) for cof, exp in self.terms])

    def __call__(self, x):
        """Implements evaluation for Polynomial objects.

        >>> p = P('x^2 + 3x^6 + 2x + 3')
        >>> p(0)
        3
        >>> p(1)
        9
        """
        return self.asFunction()(x)

    def __getitem__(self, i):
        """Implements indexing for Polynomial objects, returning by degree.

        >>> p = P('1 + 5x^3 + 3x^2')
        >>> p[2]  # return term with degree 2
        3x^2
        """
        return P([(c, e) for c, e in self.terms if e == i])


class Fraction(object):
    """Representation of a Fraction, accepts Fractions and Polynomials as
    numerators and denominators, in addition to regular numerical expressions
    in Python.
    """

    def __init__(self, num, den, sigfigs=2):
        assert den != 0
        assert isNumeric(num) or isinstance(num, Polynomial)
        assert isNumeric(den) or isinstance(den, Polynomial)
#CHECK FOR SIMPLIFIYING FRACTIONS
        self.num = poly(num)
        self.den = poly(den)
        self.sigfigs = sigfigs
        self.reduceConstants()
        self.simplify()

    def copy(self):
        return Fraction(self.num, self.den)

    def __add__(f1, f2):
        """Implements addition for Fraction object"""
        assert isNumeric(f1) and isNumeric(f2)
        if not isinstance(f2, Fraction):
            return Fraction(f1.num * f2, f1.den)
        return Fraction(f1.num * f2.den + f2.num * f1.den, f1.den * f2.den)

    def __mul__(f1, f2):
        """Implements multiplication for Fraction object"""
        assert isNumeric(f1) and isNumeric(f2)
        if not isinstance(f2, Fraction):
            return Fraction(f1.num * f2, f1.den)
        return Fraction(f1.num * f2.num, f1.den * f2.den)

    def __div__(f1,f2):
        """Implements division for Fraction object"""
        assert isNumeric(f1) and isNumeric(f2)
        return f1 * f2.copy().invert()

    def reciprocal(self):
        """Returns a new Fraction, reciprocal of the original"""
        return Fraction(self.den, self.num)

    def invert(self):
        """Inverts existing Fraction, in-place"""
        self.num, self.den = self.den, self.num
        return self

    def reduceConstants(self):
        constants = [x[0] for x in self.num.terms] + [x[0] for x in self.den.terms]
        def gcd_help(x,y):
            x,y = map(abs, (x,y))
            if y > x:
                return gcd_help(y,x)
            if y == 0:
                return x
            return gcd_help(y, x % y)
        gcd = int(reduce(gcd_help, constants)) or 1
        self.num = Polynomial([(x[0]*1.0/gcd , x[1]) for x in self.num.terms])
        self.den = Polynomial([(x[0]*1.0/gcd , x[1]) for x in self.den.terms])
        return self

    def simplify(self):
        """Takes several steps to simplify fractions:
        1. Rids of negative denominators
        2. Divides by gcd of terms in numerator and terms in denominator
        """
        if self.den.is_constant() and float(self.den) == -1:
            self.den, self.num = poly(1), -self.num

    def __repr__(self):
        return str(self)

    def __str__(self):
        """String representation of a Fraction

        >>> F(1, 3)
        1/3
        >>> F(1.4, 2)
        1.4/2
        >>> F(poly(1), poly(3))
        1/3
        >>> F(P('5x^3'), 3)
        5x^3/3
        """
        num, den = self.num, self.den
        pnum, pden = poly(num), poly(den)
        num_is_single, num_is_constant = len(pnum.terms) == 1,pnum.is_constant()
        den_is_single, den_is_constant = len(pden.terms) == 1,pden.is_constant()
        if den_is_constant and float(den) == 1:
            return str(num)
        if num_is_single and den_is_single:
            num = pint(float(num)) if num_is_constant else P(num.terms[0])
            den = pint(float(den)) if den_is_constant else P(den.terms[0])
            return "%s/%s" % (str(num), str(den))
        return "(%s)/(%s)" % (str(num), str(den))


# abbreviations
P = Polynomial
F = Fraction
