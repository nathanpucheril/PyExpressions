import utils
from utils import reduce, list_to_listOfTypes
import math
from copy import deepcopy

# @author: Alvin Wan and Nathan Pucheril


class Expression(object):

    def __init__(self, expressions):
        self.termsList = None
        self.set_terms(utils.termsFlattener(expressions))
        self.c = 1

    @property
    def terms(self):
        return self.termsList

    def set_terms(self, terms):
        assert isinstance(terms, list), "Terms must be a list"
        assert all(map(Expression.isExpression, terms)
                ), "All Terms in an Expression must be an Expression"
        self.termsList = utils.termsFlattener(terms)
        if self.termsList == []:
            self.termsList = [0]
        return self.terms

    @property
    def coefficient(self):
        return self.c

    def set_coefficient(self, coefficient):
        self.c = coefficient
        return self

    def copy(self):
        return deepcopy(self)

    @staticmethod
    def _expressify(e):
        return Expression([e])

    def simplify(self):
        for term in self.terms:
            lst = []
            if utils.isnumeric(term):
                lst.append(term)
            else:
                lst.append(term.simplify())
        self.set_terms(list(filter(lambda x: x, self.terms))) ## NEED TO FIX
        return self

    def __call__(self, *args):
        assert all(map(lambda x: utils.isnumeric(x[1]), args))
        return self.evaluate(*args)

    def __add__(e1, e2):
        assert map(Expression.isExpression, (e1, e2)
                   ), "Arguements must be an Expression"
        return Expression([e1 , e2])

    def __radd__(e1,e2):
        return e1.__add__(e2)

    def __sub__(e1, e2):
        # ERROR HANDLING DONE IN ADD
        return e1 + -e2

    def __rsub__(e1,e2):
        return e1.__sub__(e2)

    def __mul__(e1, e2):
        assert map(Expression.isExpression, (e1, e2)
                   ), "Arguements must be of Class Term"
        multiplied = 0
        for term1 in [e1] if utils.isnumeric(e1) else e1.terms:
            for term2 in [e2] if utils.isnumeric(e2) else e2.terms:
                multiplied += (term1 * term2)
        return multiplied

    def __rmul__(e1, e2):
        return e1.__mul__(e2)

    def __div__(e1, e2):
        """ p1  divided by p2 -> p1/p2 """
        # Error Handling done in Mul
        if utils.isnumeric(e2):
            return e1 * (1.0/e2)
        return e1 * e2.reciprocal()

    def __rdiv__(e1,e2):
        return e1.__div__(e2)

    def __truediv__(e1,e2):
        return e1.__div__(e2)

    def __rtruediv__(e1,e2):
        return e1.__truediv__(e2)

    def __floordiv__(e1,e2):
        return e1.__truediv__(e2)

    def __rfloordiv__(e1,e2):
        return e1.__truediv__(e2)

    def __neg__(self):
        # ERROR HANDLING DONE IN MUL
        return self.copy() * -1

    def __pow__(e1, e2):
        return PowerTerm(1, e1, e2)

    def __rpow__(e1, e2):
        return e1.__pow__(e2)

    def reciprocal(self):
        return Fraction._fractifyExpression(self).invert()

    def invert(self):
        # Not In place because if it isnt of type fraction, must return a
        # fraction
        return self.reciprocal()

    def derivative(self):
        return Expression([term.derivative for term in self.terms])

    @staticmethod
    def isZero(self):
        if str(self) == "0":
            return True
        return False

    @staticmethod
    def isExpression(e):
        return isinstance(e, Expression) or utils.isnumeric(e)

    def evaluate(self, *args):
        evaluated = []
        for term in self.termsList:
            if utils.isnumeric(term):
                evaluated.append(term)
            else:
                evaluated.append(term(*args))

        return sum(evaluated)

    def toPolynomial(self):
        return Polynomial(self.terms)

    def __eq__(e1, e2):
        return str(e1) == str(e2) or (e1 - e2).isZero() or str((e1 - e2).isZero()) # FIX THIS

    def __str__(self):
        output = str(self.c) if self.c != 1 else ""
        for term in self.terms:
            output += str(term) + " + "
        return output[:len(output) - 3]


class Fraction(Expression):

    def __init__(self, num, den):
        assert map(Expression.isExpression, (num, den))
        assert not Expression.isZero(den)
        # CHECK FOR SIMPLIFIYING FRACTIONS
        self.num = Expression._expressify(num)
        self.den = Expression._expressify(den)
        self.reduceCoefficients()

    @property
    def terms(self):
        return [self]

    @staticmethod
    def _fractifyExpression(e):
        if Fraction.isFraction(e):
            return e
        return Fraction(e, 1)

    def simplify(self):
        return Fraction(self.num.simplify(),
            self.den.simplify()).reduceCoefficients()

    def __add__(f1, f2):
        assert isinstance(f1, Expression) and isinstance(f2, Expression)
        f1 = Fraction._fractifyExpression(f1)
        f2 = Fraction._fractifyExpression(f2)
        return Fraction(f1.num * f2.den + f2.num * f1.den, f1.den * f2.den)

    def __mul__(f1, f2):
        assert isinstance(f1, Expression) and isinstance(f2, Expression)
        f1 = Fraction._fractifyExpression(f1)
        f2 = Fraction._fractifyExpression(f2)
        return Fraction(f1.num * f2.num, f1.den * f2.den)

    def __div__(f1, f2):
        f1 = Fraction._fractifyExpression(f1)
        f2 = Fraction._fractifyExpression(f2)
        return f1 * f2.copy().invert()

    def invert(self):
        self.num, self.den = self.den, self.num
        return self

    def reciprocal(self):
        return Fraction(self.den, self.num)

    def reduceCoefficients(self):
        allTerms = self.num.terms + self.den.terms
        coefficients = list(
            term if utils.isnumeric(term) else term.coefficient
                for term in allTerms)

        def gcd_help(x, y):
            x, y = map(abs, (x, y))
            if y > x:
                return gcd_help(y, x)
            if y == 0:
                return x
            return gcd_help(y, x % y)
        gcd = reduce(gcd_help, coefficients)
        for term in allTerms:
            if utils.isnumeric(term):
                term /= gcd
            else:
                term.set_coefficient(term.coefficient / gcd)
        return self

    def evaluate(self, *args):
        return self.num(*args) / self.den(*args)

    @staticmethod
    def isFraction(f):
        return isinstance(f, Fraction)

    def __str__(self):
        if str(self.den) == "1.0" or str(self.den) == "1":
            return str(self.num)
        return "(" + str(self.num) + ")/(" + str(self.den) + ")"


class Polynomial(Expression):

    def __init__(self, terms):
        assert isinstance(terms, list), "Terms must be a list "
        for term in terms:
            assert PowerTerm.isPwrTerm(term) or utils.isnumeric(term), "Every Term must be a PowerTerm" + " " + str(term)
            assert utils.isnumeric(term) or utils.isnumeric(term.exp), "Every Term must be a PowerTerm"
        super(Polynomial, self).__init__(terms)
        self.set_terms(terms)
        self.maxDegree = None


    def simplify(self):
        newTerms = []
        for term in self.terms:
            if utils.isnumeric(term):
                if term != 0:
                    newTerms.append(term)
            elif term.c != 0:
                newTerms.append(term)
        self.set_terms(newTerms)
        return self._combine_terms()._sort()

    def _sort(self):
        degree_fn = lambda x: 0 if utils.isnumeric(x) else x.exp
        self.set_terms(sorted(self.terms, key = degree_fn, reverse = True))
        return self

    def _combine_terms(self):
        degree_fn = lambda x: 0 if utils.isnumeric(x) else x.exp
        sep_degrees = utils.list_to_listOfTypes(self.terms, degree_fn)
        self.set_terms(list(map(sum, sep_degrees)))
        return self

    def __add__(p1, p2):
        added = Expression.__add__(p1,p2)
        return Polynomial.polify(added) if p2.__class__ == Polynomial else added

    def __mul__(p1, p2):
        mult = Expression.__mul__(p1,p2)
        return Polynomial.polify(mult) if p2.__class__ == Polynomial else mult

    def __mod__(p1, p2):
        assert Polynomial.isPolynomial(p2)
        return Polynomial._divider(p1,p2, "modulo")

    def __rmod__(p1, p2):
        return p1.__mod__(p2)

    def __div__(e1, e2):
        """ p1  divided by p2 -> p1/p2 """
        return p1.__truediv__(p2)

    def __truediv__(e1,e2):
        assert Polynomial.isPolynomial(p2)
        return p1._divider(p2)

    def __floordiv__(e1,e2):
        assert Polynomial.isPolynomial(p2)
        return p1._divider(p2, "floordiv")

    def isZero(self):
        self.simplify()
        return self.terms == [0] or str(self) == "0"

    def _divider(p1, p2, op = "truediv"):
        p1 = Polynomial.polify(p1).simplify()
        p2 = Polynomial.polify(p2).simplify()

        if p1.isZero():
            return 0
        if p1.degree < p2.degree:
            if op == "truediv":
                return Fraction(p1 , p2)
            if op == "floordiv":
                return 0
            if op == "modulo":
                return p1
        divisorTerms = []

        p1Term1 = p1.terms[0]
        p1Term1_coeff = p1Term1 if utils.isnumeric(p1Term1) else p1Term1.c
        p2Term2 = p2.terms[0]
        p2Term1_coeff = p2Term2 if utils.isnumeric(p2Term2) else p2Term2.c

        divisorVal = PowerTerm(p1Term1_coeff / p2Term1_coeff, x, p1.degree - p2.degree)

        divisorTerms.append(divisorVal)
        newP1 = Polynomial.polify(p1 - (p2 * divisorVal)).simplify()
        divisorTerms.append(newP1._divider(p2, op))
        if op == "modulo":
            return divisorTerms.pop()
        return sum(divisorTerms)

    @staticmethod
    def polify(p):
        if Polynomial.isPolynomial(p):
            return p
        return Polynomial([p]) if utils.isnumeric(p) else Polynomial(p.terms)

    @property
    def degree(self):
        degree_fn = lambda x: 0 if utils.isnumeric(x) else x.exp
        return max(list(map(degree_fn, self.terms)))

    @staticmethod
    def isPolynomial(p):
        return isinstance(p, Polynomial)

    # Done
    def __str__(self):
        output = ""
        for term in self.terms:
            c = exp = 0
            if utils.isnumeric(term):
                c = term
            else:
                c, exp = term.c, term.exp
            if c == 0:
                continue
            elif exp == 0:
                output += str(c) + " + "
            elif c == 1:
                output += "x^" + str(exp) + " + "
            else:
                output += str(c) +  "x^" + str(exp) + " + "
        output = output[: len(output) - 3]
        if output == "":
            return "0"
        return output

##############################


class Term(Expression):

    def __init__(self):
        super(Term, self).__init__([self])

    @property
    def terms(self):
        return [self]

    def __mul__(m1, m2):
        assert map(Term.isTerm, (m1, m2)
                   ), "Arguements must be of type ExponentialTerm"
        return MultTerm([m1, m2])

    def simplify(self):
        return self

    @staticmethod
    def isTerm(t):
        return isinstance(t, Term)


class X(Term):

    def __init__(self, var="x"):
        super(X, self).__init__()
        self.var = var

    def evaluate(self, *args):
        for arg in args:
            if arg[0].var == self.var:
                return arg[1]

    def __add__(x1, x2):
        assert map(Expression.isExpression, (x1, x2)
                   ), "Arguements must be an Expression"
        if x1.__class__ == x2.__class__:
            return PowerTerm(2, x1, 1)
        if utils.isnumeric(x2):
            return Expression(x1.terms + [x2])
        return Expression(x1.terms + x2.terms)

    def __mul__(x1, x2):
        assert map(Expression.isExpression, (x1, x2)
                   ), "Arguements must be an Expression"
        if x2.__class__ == Expression:
            return x2.__mul__(x1)
        if x1.__class__ == x2.__class__:
            return PowerTerm(1, x1, 2) if x1.var == x2.var else MultTerm((x1, x2))
        if utils.isnumeric(x2):
            return PowerTerm(x2, x1, 1)
        return x2.__mul__(x1)

    def __neg__(self):
        # ERROR HANDLING DONE IN MUL
        return PowerTerm(-1, self, 1)

    def __str__(self):
        return self.var

    @staticmethod
    def isX(x):
        return isinstance(x, X)

    def derivative(self):
        return 1


class MultTerm(Term):

    def __init__(self, terms):
        super(MultTerm, self).__init__()
        self.termsMultiplied = list(terms)
        self.c = 1
        for term in self.termsMultiplied:
            if MultTerm.isMultTerm(term):
                self.termsMultiplied.extend(term.termsMultiplied)
                self.c *= term.c
                self.termsMultiplied.remove(term)

        for term in self.termsMultiplied:
            if utils.isnumeric(term):
                self.c *= term
                self.termsMultiplied.remove(term)
            else:
                self.c *= term.c
                term.set_coefficient(1)

    def __mul__(m1, m2):
        assert map(Expression.isExpression, (m1, m2)
                   ), "Arguements must be of type ExponentialTerm"
        if m2.__class__ == Expression:
            return m2 * m1
        if m1.__class__ == m2.__class__:
            return MultTerm(m1.termsMultiplied + m2.termsMultiplied)
        return MultTerm([m1, m2])

    def simplify(self):
        if self.c == 0:
            return 0
        else:
            self.termsMultiplied = [term.simplify() for term in self.termsMultiplied]
        return self

    def evaluate(self, *args):
        product = 1
        for term in self.termsMultiplied:
            product *= term(*args)
        return product

    @staticmethod
    def isMultTerm(m):
        return isinstance(m, MultTerm)

    def __str__(self):

        output = "" if self.c == 1 else str(self.c) + "*"
        for term in self.termsMultiplied:
            output += "(" + str(term) + ")" + "*"
        return  output[: len(output) - 1]


class PowerTerm(Term):

    def __init__(self, coefficient=1, main = X(), exp=1):
        super(PowerTerm, self).__init__()
        assert utils.isnumeric(coefficient), "Coefficient must be a number"
        assert Expression.isExpression(main)
        assert Expression.isExpression(exp)
        self.c = coefficient
        self.main = main
        self.exp = exp

    def set_exp(self, exp):
        self.exp = exp
        return self

    def simplify(self):
        return self._simplifier()

    def _simplifier(self):
        if self.exp == 0:
            return self.c
        if self.c == 0:
            return 0
        return self

    def __add__(p1, p2):
        assert map(Expression.isExpression, (p1, p2)
                   ), "Arguements must be of type ExponentialTerm"
        if p2 == 0:
            return p1
        if p1.exp == 0 and utils.isnumeric(p2):
            return p2 + p1.c
        if PowerTerm.isPwrTerm(p2) and p1.exp == p2.exp and p1.main == p2.main: # ERROR HERE WITH VAR
            return PowerTerm(p1.c + p2.c, p1.main, p1.exp)
        else:
            return Expression([p1, p2])

    def __mul__(p1, p2):
        assert map(Expression.isExpression, (p1, p2)
                   ), "Arguements must be of type ExponentialTerm"
        if p2.__class__ == Expression:
            return p2.__mul__(p1)
        if p1.__class__ == p2.__class__ and p1.main == p2.main:
            return PowerTerm(p1.c * p2.c, p1.main, p1.exp + p2.exp)
        if p2 == p1.main:
            copy = p1.copy()
            return copy.set_exp(copy.exp + 1)
        if utils.isnumeric(p2):
            return PowerTerm(p1.c * p2, p1.main, p1.exp)
        if p2.__class__ == Fraction:
            return p2.__mul__(p1)

        return MultTerm([p1, p2])

    def evaluate(self, *args):
        m, e = self.main, self.exp
        mainVal = m if utils.isnumeric(m) else m(*args)
        expVal = e if utils.isnumeric(e) else e(*args)
        return self.c * pow(mainVal, expVal)

    @staticmethod
    def isPwrTerm(p):
        return isinstance(p, PowerTerm)

    def __str__(self):
        o_paran = "(" if not Term.isTerm(self.main) else ""
        c_paran = ")" if not Term.isTerm(self.main) else ""

        output = ""

        main = str(self.main)
        exp = "^" + str(self.exp)
        coefficient = str(self.c)

        if self.c == 0:
            return "0"
        if self.exp == 0:
            return str(self.c)
        if self.exp == 1:
            exp = ""
        if self.c == 1:
            return o_paran + main + c_paran + exp
        return o_paran + coefficient + main + c_paran + exp


class ExponentialTerm(Term):

    def __init__(self, coefficient=1, exp=1):
        super(ExponentialTerm, self).__init__()
        assert isinstance(exp, Expression)
        self.c = coefficient
        self.exp = exp

    def __add__(e1, e2):
        assert map(ExponentialTerm.isExpTerm, (e1, e2)
                   ), "Arguements must be of type ExponentialTerm"
        if e2 == 0:
            return e1
        if e1.exp == e2.exp:
            return ExponentialTerm(e1.c + e2.c, e1.exp)
        else:
            return Expression([e1, e2])

    def simplify(self):
        if self.c == 0:
            return 0
        return self

    def __mul__(e1, e2):
        assert map(Expression.isExpression, (e1, e2)
                   ), "Arguements must be of type ExponentialTerm"
        if e2.__class__ == Expression:
            return e2.__mul__(e1)
        if ExponentialTerm.isExpTerm(e2):
            return ExponentialTerm(e1.c * e2.c, e1.exp + e2.exp)
        if e2.__class__ == X:
            e2 = PowerTerm(1, p2, 1)
        return MultTerm([e1, e2])

    def evaluate(self, *args):
        expVal = self.exp if utils.isnumeric(self.exp) else self.exp(*args)
        return self.c * math.exp(expVal)

    def isExpTerm(e):
        return isinstance(e, ExponentialTerm)

    def __str__(self):
        if self.exp == 0:
            return str(self.c)
        if self.c == 0:
            return "0"
        else:
            return str(self.c) + "e" + "^(" + str(self.exp) + "x)"


class LogTerm(Term):
    """c*log_base(Beta * x)"""

    def __init__(self, insideTerm, coefficient=1, base=10):
        super(LogTerm, self).__init__()
        assert Expression.isExpression(insideTerm)
        assert Expression.isExpression(base)
        self.c = coefficient
        self.base = base
        self.insideTerm = insideTerm

    def __mul__(l1, l2):
        assert map(Expression.isExpression, (e1, e2)
                   ), "Arguements must be of type ExponentialTerm"
        if x2.__class__ == Expression:
            return x2.__mul__(x1)
        if LogTerm.isLogTerm(l2) and l2.base == l1.base:
            return LogTerm(PowerTerm(1,l1.insideTerm,l1.c) + PowerTerm(1,l2.insideTerm, l2.c),1, l1.base)
        if e2.__class__ == X:
            e2 = PowerTerm(1, p2, 1)
        return MultTerm((l1, l2))

    def simplify(self):
        self.insideTerm = self.insideTerm.simplify()
        return self

    def evaluate(self, *args):
        i, b = self.insideTerm, self.base
        insideVal = i if utils.isnumeric(i) else i(*args)
        baseVal = b if utils.isnumeric(b) else b(*args)
        return self.c * math.log(insideVal, baseVal)

    @staticmethod
    def isLogTerm(l):
        return isinstance(l, LogTerm)

    def __str__(self):
        base = "e" if self.base == math.e else str(self.base)
        coeff = str(self.c) if self.c != 1 else ""
        return coeff + "log_" + base + "(" + str(self.insideTerm) + ")"


x = X()
# p = Polynomial([x**2, x**2])
# print(p._combine_terms().terms[0])
# p1 = Polynomial.polify( x**2 )
# p2 = Polynomial.polify(PowerTerm(1, x, 1) + 1)
# div = p1 % p2
# print(div)
