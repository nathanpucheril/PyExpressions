import utils
import math

# @author: Nathan Pucheril


class ExpressionBuilder(object):

    @staticmethod
    def parse(string):
        pass

    @staticmethod
    def make_polynomial(terms):
        return Polynomial(terms)

    def __div__(num, den):
        ##### handle cases of inserting constants and expressions
        return Fraction(num, den)

    @staticmethod
    def x(p):
        return PowerTerm(1, X(), p)

    @staticmethod
    def log(x):
        # if not Expression.isExpression(x):
        #     Expressify

        return LogTerm(x)

    @staticmethod
    def ln(x):
        return LogTerm(x, 1, math.e)

    @staticmethod
    def log_base(base, x):
        return LogTerm(x, 1, base)

    def __add__(x, y):
        return x + y

    def __mul__(x, y):
        return x * y

    @staticmethod
    def exp(x):
        return ExponentialTerm(1, x)

    @staticmethod
    def cons(constant):
        return ConstantTerm(constant)

    @staticmethod
    def coefficient(coefficient, term):
        c = Expression.make_constant(constant)
        term.set_coefficient(c)
        return term

#############################

class Expression(object):
    def __init__(self, expressions, var = "x"):
        assert isinstance(expressions, list), "Terms must be a list"
        assert isinstance(var, str), "The Variable must be a string representing the Variable"
        # assert map(Term.isTerm, terms), "All Terms in an Expression must be of the Class Term"
        self.var = var
        self.termsList = expressions
        self.c = 1

    @property
    def terms(self):
        return self.termsList

    def copy(self):
        return deepcopy(self)

    def _clean(self):
        cleaned = []
        for term in self.terms:
            c = term.coefficient
            if c == 0:
                continue
            cleaned.append(term)
        self.terms = cleaned
        return self

    def __call__(self, x):
        assert utils.isnumeric(x)
        return self.evaluate(x)

    def __add__(e1, e2):
        assert map(Expression.isExpression, (e1, e2)), "Arguements must be an Expression"
        assert e1.var == e2.var, "Single Variable Expressions Only"
        return Expression(e1.terms + e2.terms, e1.var )

    def __sub__(e1,e2):
        # ERROR HANDLING DONE IN ADD
        return e1 + -e2

    def __mul__(e1, e2):
        assert map(Expression.isExpression, (e1, e2)), "Arguements must be of Class Term"
        assert e1.var == e2.var, "Single Variables Expressions Only"

        ##MAYBE USE A REDUCE HER
        multiplied = []
        for term1 in e1.terms:
            for term2 in e2.terms:
                multiplied.append(term1 * term2)
        return Expression(multiplied)

    def __div__(e1, e2):
        """ p1  divided by p2 -> p1/p2 """
        #Error Handling done in Mul
        return e1 * e2.reciprocal()

    def __neg__(self):
        # ERROR HANDLING DONE IN MUL
        return self.copy() * ConstantTerm(-1)

    def reciprocal(self):
        return Fraction._fractifyExpression(self).invert()

    def invert(self):
        # Not Inplace because if it isnt of type fraction, must return a fraction
        return self.reciprocal()

    def isZero(self):
        return False

    @staticmethod
    def isExpression(e):
        return isinstance(e, Expression)

    @staticmethod
    def make_constant(c):
        return Expression([PowerTerm(c, X(), 0)])

    def evaluate(self, x):
        return sum([term(x) for term in self.termsList])

    def __str__(self):
        output = ""
        for term in self.terms:
            output += str(term) + " + "
        return output[:len(output) - 3]

class Fraction(Expression):

    def __init__(self, num, den, var = "x"):
        assert map(Expression.isExpression, (num, den))
        assert not Expression.isZero(den)
        #CHECK FOR SIMPLIFIYING FRACTIONS
        self.var = var
        self.num = num
        self.den = den
        self.reduceCoefficients()

    @property
    def terms(self):
        return [self]

    def copy(self):
        return deepcopy(self)

    @staticmethod
    def _fractifyExpression(e):
        if Fraction.isFraction(e):
            return e
        return Fraction(e, ConstantTerm(1))

    def __add__(f1, f2):
        assert isinstance(f1, Expression) and isinstance(f2, Expression)
        f1 = Fraction._fractifyExpression(f1)
        f2 = Fraction._fractifyExpression(f2)
        return Fraction(f1.num * f2.den + f2.num * f1.den, f1.den * f2.den)

    def __mul__(f1,f2):
        assert isinstance(f1, Expression) and isinstance(f2, Expression)
        f1 = Fraction._fractifyExpression(f1)
        f2 = Fraction._fractifyExpression(f2)
        return Fraction(f1.num * f2.num, f1.den * f2.den)

    def __div__(f1,f2):
        assert isinstance(f1, Fraction) and isinstance(f2, Fraction)
        return f1 * f2.copy().invert()

    def __neg__(f1):
        return Fraction(-self.num, self.den)

    def invert(self):
        self.num, self.den = self.den, self.num
        return self

    def reciprocal(self):
        return Fraction(self.den, self.num, self.var)

    def reduceCoefficients(self):
        coefficients = list(term.coefficient for term in self.num.terms + self.den.terms)
        def gcd_help(x,y):
            x,y = map(abs, (x,y))
            if y > x:
                return gcd_help(y,x)
            if y == 0:
                return x
            return gcd_help(y, x % y)
        gcd = reduce(gcd_help, coefficients)
        for x in self.num.terms:
            x.set_coefficient(x.coefficient/gcd)
        for x in self.den.terms:
            x.set_coefficient(x.coefficient/gcd)
        return self

    def evaluate(self, x):
        return self.num(x) / self.den(x)

    @staticmethod
    def isFraction(f):
        return isinstance(f, Fraction)

    def __str__(self):
        if str(self.den) == "1.0" or str(self.den) == "1":
            return str(self.num)
        return "(" + str(self.num) + ")/(" + str(self.den) + ")"

class Polynomial(Expression):

    def __init__(self, terms, var = "x"):
        assert isinstance(terms, list), "Terms must be a list of tuples"
        assert isinstance(var, str), "The Variable must be a string representing the Variable"
        for term in terms:
            assert isinstance(term, PowerTerm), "Every Term must be a PowerTerm"
            assert isinstance(term.exp, ConstantTerm), "Every terms exponent must be a ConstantTerm"
        super(Polynomial, self).__init__(var)
        self.termsList = self.unsimplified_terms = terms
        self._clean()


    def _clean(self):
        self._combine_terms()._simplify()._sort()
        return self

    def sort(self):
        self.termsList = sorted(self.terms, key = lambda x: x[1],reverse = True)
        return self

    def _combine_terms(self):
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

    def _simplify(self):
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

##############################

class Term(Expression):
    def __init__(self, var = "x"):
        super(Term, self).__init__([self], var)

    @property
    def terms(self):
        return [self]

    @property
    def coefficient(self):
        return self.c

    def copy(self):
        return deepcopy(self)

    def set_coefficient(self, coefficient):
        self.c = coefficient
        return self

    def __add__(t1, t2):
        assert map(Term.isTerm, (t1,t2)), "Arguements must be of type ExponentialTerm"
        if t1.__class__ == t2.__class__:
            return t1 + t2
        return Expression([t1, t2])

    def __mul__(t1, t2):
        assert map(Term.isTerm, (t1,t2)), "Arguements must be of type ExponentialTerm"
        if t1.__class__ == t2.__class__:
            return t1 * t2
        return Expression([MultTerm(t1,t2)])

    @staticmethod
    def isTerm(t):
        return isinstance(t, Term)

class ConstantTerm(Term):
    def __init__(self, constant = 1, var = "x"):
        assert utils.isnumeric(constant), "ConstantTerm Takes in a numeric value"
        super(ConstantTerm, self).__init__(var)
        self.constant = constant
        self.c = 1

    def __call__(self, x):
        assert utils.isnumeric(x)
        return self.constant

    def evaluate(self, x):
        return self.constant

    def __add__(c1, c2):
        assert map(Expression.isExpression, (c1, c2)), "Arguements must be an Expression"
        assert c1.var == c2.var, "Single Variable Expressions Only"
        if type(c1) == type(e2):
            return ConstantTerm(e1.constant + e2.constant, e1.var)
        return Expression(e1.terms + e2.terms, e1.var )

    def __mul__(c1, c2):
        assert map(Expression.isExpression, (c1, c2)), "Arguements must be of Class Term"
        assert c1.var == c2.var, "Single Variables Expressions Only"
        if c1.__class__ == c2.__class__:
            return ConstantTerm(c1.constant * c2.constant, c2.var)
        return Expression([MultTerm((c1,c2), c1.var)])

    def __neg__(self):
        # ERROR HANDLING DONE IN MUL
        return ConstantTerm(-self.constant, self.var)

    def __str__(self):
        return str(self.constant)

class X(Term):
    def __init__(self, var = "x"):
        super(X, self).__init__(var)


    def evaluate(self, x):
        return x

    def __add__(x1, x2):
        assert map(Expression.isExpression, (c1, c2)), "Arguements must be an Expression"
        assert x1.var == x2.var, "Single Variable Expressions Only"
        if x1.__class__ == x1.__class__:
            return PowerTerm(1, X(), 2, x1.var)
        return Expression(x1.terms + x2.terms, x1.var)

    def __mul__(c1, c2):
        assert map(Expression.isExpression, (c1, c2)), "Arguements must be of Class Term"
        assert c1.var == c2.var, "Single Variables Expressions Only"
        if c1.__class__ == c2.__class__:
            return ConstantTerm(c1.constant * c2.constant, c2.var)
        return Expression([MultTerm((c1,c2), c1.var)])

    def __neg__(self):
        # ERROR HANDLING DONE IN MUL
        return PowerTerm(-1, X(), 1, x1.var)

    def __str__(self):
        return self.var

class MultTerm(Term):
    def __init__(self, terms, var = "x"):
        super(MultTerm, self).__init__(var)
        self.termsMultiplied = terms
        self.c = 1

    def evaluate(self, x):
        product = 1
        for term in self.termsMultiplied:
            product *= term.evaluate(x)
        return product

    def __str__(self):
        output = ""
        total_coefficient = 1
        for term in self.termsMultiplied:
            total_coefficient *= term.c
            output += (str(term) + "*")[len(str(term.c)):]
        return str(total_coefficient) + output[: len(output) - 1]

class PowerTerm(Term):
    def __init__(self, coefficient = 1, main = X(), exp = ConstantTerm(1), var = "x"):
        super(PowerTerm, self).__init__(var)
        assert utils.isnumeric(coefficient), "Coefficient must be a number"
        assert isinstance(main, Expression)
        assert isinstance(exp, Expression)
        self.c = coefficient
        self.main = main
        self.exp = exp

    def __add__(p1, p2):
        assert map(Expression.isExpression, (p1,p2)), "Arguements must be of type ExponentialTerm"
        if p1.exp == p2.exp and p1.main == p2.main:
            return PowerTerm(p1.c + p2.c, p1.main, p1.exp)
        else:
            return Expression([p1,p2])

    def __mul__(p1, p2):
        assert map(Expression.isExpression, (p1,p2)), "Arguements must be of type ExponentialTerm"
        if type(p1) == type(p2) and p1.exp == p2.exp and p1.main == p2.main:
            return PowerTerm(p1.c * p2.c, p1.main, p1.exp + p2.exp)
        return MultTerm([p1,p2])

    def __neg__(p1):
        return PowerTerm(-p1.c, p1.main, p1.exp)

    def evaluate(self, x):
        return self.c * pow(self.main(x), self.exp(x))

    def isPwrTerm(p):
        return isinstance(p, PowerTerm)

    def __str__(self):
        if self.exp == 0:
            return str(self.c)
        if self.c == 0:
            return "0"
        if self.c == 1:
            return self.var + "^" + str(self.exp)
        return str(self.c) + self.var + "^" + str(self.exp)

class ExponentialTerm(Term):
    def __init__(self, coefficient = 1, exp = 1, var = "x"):
        super(ExponentialTerm, self).__init__(var)
        assert isinstance(self.exp, Expression)
        self.c = coefficient
        self.exp = exp

    def __add__(e1, e2):
        assert map(ExponentialTerm.isExpTerm, (e1,e2)), "Arguements must be of type ExponentialTerm"
        if e1.exp == e2.exp:
            return ExponentialTerm(e1.c + e2.c, e1.exp)
        else:
            return Expression([e1,e2])

    def multiply(e1, e2):
        assert map(ExponentialTerm.isExpTerm, (e1,e2)), "Arguements must be of type ExponentialTerm"
        return ExponentialTerm(e1.c * e2.c, e1.exp + e2.exp)

    def __neg__(e1):
        return ExponentialTerm(-e1.c, e1.exp)

    def evaluate(self, x):
        return self.c * math.exp(self.exp(x))

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
    def __init__(self, insideTerm, coefficient = 1, base = 10, var = "x"):
        super(LogTerm, self).__init__(var)
        assert Expression.isExpression(insideTerm)
        self.c = coefficient
        self.base = base
        self.insideTerm = insideTerm


    def __add__(l1, l2):
        assert map(LogTerm.isLogTerm, (e1,e2)), "Arguements must be of type ExponentialTerm"
        return Expression([e1,e2])

    def __mul__(e1, e2):
        return MultTerm((e1,e2))

    def __neg__(l1):
        return LogTerm(-l1.c, l1.base, l1.beta)

    def evaluate(self, x):
        return self.c * math.log(self.insideTerm(x), self.base)

    def isLogTerm(e):
        return isinstance(e, LogTerm)

    def __str__(self):
        base = "e" if self.base == math.e else str(self.base)
        return str(self.c) + "log_" + base + "(" + str(self.insideTerm) + ")"
