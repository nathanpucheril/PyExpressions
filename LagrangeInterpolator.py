import numbers as numbersModule

def isnumber(arg):
    return isinstance(arg, numbersModule.Number)

def isFraction(arg):
    return isinstance(arg, Fraction)

def isPolynomial(arg):
    return isinstance(arg, Polynomial)

class Polynomial(object):

    def __init__(self, terms, var = "x"):
        assert isinstance(terms, list), "Terms must be a list of tuples"
        assert isinstance(var, str), "The Variable must be a string representing the Variable"
        self.termsList = self.unsimplified_terms = terms
        self.combine_terms()
        self.simplify()
        self.sort()
        self.var = var

    #Done
    @property
    def terms(self):
        return self.termsList

    #Done
    def copy(self):
        return Polynomial(self.terms,self.var)

    def sort(self):
        self.termsList = sorted(self.terms, key = lambda x: x[1],reverse = True)
        return self

    @staticmethod
    def make_constant(c):
        return Polynomial([(c, 0)])
    #Done
    def combine_terms(self):
        combined = {}
        for term in self.terms:
            exp = term[1]
            coefficient = term[0]
            if combined.has_key(exp):
                combined[exp] = combined[exp] +  coefficient
            else:
                combined[exp] = coefficient
        self.termsList = [(v, k) for k, v in combined.items()] # FLips Key Value Pairs
        return self

    def simplify(self):
        simplified = {}
        for term in self.terms:
            exp = term[1]
            coefficient = term[0]
            if coefficient == 0:
                continue
            else:
                simplified[exp] = coefficient
        self.termsList = [(v, k) for k, v in simplified.items()] # FLips Key Value Pairs
        return self

    #Done
    @staticmethod
    def add(p1, p2):
        p1terms = [(p1, 0)] if Polynomial.Number_or_Polynomial(p1) else p1.terms
        p2terms = [(p2, 0)] if Polynomial.Number_or_Polynomial(p2) else p2.terms
        return Polynomial(p1terms + p2terms)

    #Done
    @staticmethod
    def subtract(p1,p2):
        # ERROR HANDLING DONE IN ADD
        return Polynomial.add(p1, Polynomial.negate(p2))

    #Done
    @staticmethod
    def negate(p1):
        p1terms = [(p1, 0)] if Polynomial.Number_or_Polynomial(p1) else p1.terms

        newTerms = []
        for term in p1terms:
            c, exp = term
            newTerms.append((-c, exp))
        return Polynomial(newTerms, p1.var)

    @staticmethod
    def multiply(p1, p2):
        p1terms = [(p1, 0)] if Polynomial.Number_or_Polynomial(p1) else p1.terms
        p2terms = [(p2, 0)] if Polynomial.Number_or_Polynomial(p2) else p2.terms


        multiplied = []
        def multiplyTerms(point1, point2):
            c1,exp1 = point1
            c2,exp2 = point2
            return (c1*c2, exp1+exp2)

        multiplied = []
        for point1 in p1terms:
            for point2 in p2terms:
                multiplied.append(multiplyTerms(point1, point2))
        # print(multiplied)
        return Polynomial(multiplied)

    @staticmethod
    def divide(p1, p2):
        """ p1  divided by p2 -> p1/p2 """
        if isinstance(p1, Polynomial):
            p1 = p1.get_terms()
        if isinstance(p2, Polynomial):
            p2 = p2.get_terms()

        return Polynomial.multiply(p1, inverse_p2)

    #Done
    @staticmethod
    def poly_print(polynomial):
        output = ""
        for term in polynomial:
            output += str(term[0]) + self.var + "^" + str(term[1]) + " + "
        output = output[: len(output) - 3]
        print(output)

    def Number_or_Polynomial(p1):
        """Return 1 if Number 0 if Polynomial, else Error"""
        errMsg = "Args must be a list of  terms or instance of Polynomail!"
        assert isnumber(p1) or isFraction(p1) or isPolynomial(p1), errMsg
        return 1 if (isnumber(p1) or isFraction(p1)) else 0

    #Done
    def __str__(self):
        output = ""
        for term in self.terms:
            c, exp = term
            if c == 0:
                continue
            elif exp == 0:
                output += str(c) + " + "
            elif c == 1:
                output +=  self.var + "^" + str(term[1]) + " + "
            else:
                output += str(term[0]) + self.var + "^" + str(term[1]) + " + "
        output = output[: len(output) - 3]
        return output

class Fraction(object):
    """Utilizes Polynomial Class"""

    def __init__(self, num, den):
        assert den != 0
        assert isnumber(num) or isPolynomial(num) or isFraction(num)
        assert isnumber(den) or isPolynomial(den) or isFraction(den)
#CHECK FOR SIMPLIFIYING FRACTIONS
        self.num = Polynomial.make_constant(num) if isnumber(num) else num
        self.den = Polynomial.make_constant(den) if isnumber(den) else den
        self.reduceConstants()

    def copy(self):
        return Fraction(self.num, self.den)

    @staticmethod
    def add(f1, f2):
        assert isinstance(f1, Fraction) and isinstance(f2, Fraction)
        common_den = Polynomial.multiply(f1.den, f2.den)
        f1num = Polynomial.multiply(f1.num, f2.den)
        f2num = Polynomial.multiply(f2.num, f1.den)
        return Fraction(Polynomial.add(f1num, f2num), common_den)

    @staticmethod
    def multiply(f1,f2):
        assert isinstance(f1, Fraction) and isinstance(f2, Fraction)
        return Fraction(Polynomial.multiply(f1.num, f2.num), Polynomial.multiply(f1.den, f2.den))

    @staticmethod
    def divide(f1,f2):
        assert isinstance(f1, Fraction) and isinstance(f2, Fraction)
        return Fraction.multiply(f1, f2.copy().invert())

    def invert(self):
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
        gcd = reduce(gcd_help, constants)
        self.num = Polynomial([(x[0]*1.0/gcd , x[1]) for x in self.num.terms])
        self.den = Polynomial([(x[0]*1.0/gcd , x[1]) for x in self.den.terms])
        return self

    def __repr__(self):
        return str(self)

    def __str__(self):
        if str(self.den) == "1.0" or str(self.den) == "1":
            return str(self.num)
        return "(" + str(self.num) + ")/(" + str(self.den) + ")"


def L_Interpolate(*points):
    """takes in a list of tuples of x,y cordinates

    >>> L_Interpolate((-1, 1), (0, 0), (1, 1))
    x^2
    >>> interpolate = lambda y, d: L_Interpolate(*[
    ... (x, y(x)) for x in range(d+1)])
    >>> y2 = lambda x: x**5 + 3*x**3 + x
    >>> interpolate(y2, 5)
    x^5 + 3.0x^3 + x^1
    >>> y3 = lambda x: 7*x**4 + 5*x**3 + 9*x**2 + 3
    >>> interpolate(y3, 4)
    7.0x^4 + 5.0x^3 + 9.0x^2 + 3.0
    """
    poly = Polynomial
    poly_builder = []

    final_polynomial = Fraction(Polynomial([(0, 0)]),1)
    for point in points:
        x_point1, y_point1 = point
        num = poly.make_constant(1)
        den = poly.make_constant(1)

        for point2 in points:
            if point == point2:
                continue
            x_point2 = point2[0]
            num = poly.multiply(num, Polynomial([(1, 1), (-x_point2,0)]))
            den = poly.multiply(den, Polynomial([(x_point1, 0), (-x_point2,0)]))
            numden = Fraction.multiply(Fraction(num, 1), Fraction(1,den))
        poly_builder.append(Fraction.multiply(numden, Fraction(y_point1,1)))

    for polynomial in poly_builder:
        final_polynomial = Fraction.add(final_polynomial, polynomial)
    return final_polynomial
