from copy import deepcopy
import numbers as numbersModule
import math
from functools import reduce

class ExpressionBuilder(object):

    @staticmethod
    def str_to_expression(string):
        pass

    @staticmethod
    def make_polynomial(terms):
        return Polynomial(terms)

    @staticmethod
    def fract(num, den):
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

    @staticmethod
    def add(x, y):
        return Expression.add(x, y)

    @staticmethod
    def mult(x, y):
        return Expression.multiply(x, y)

    @staticmethod
    def exp(x):
        return ExponentialTerm(1, x)

    @staticmethod
    def cons(constant):
        return Expression.make_constant(constant)

    @staticmethod
    def coefficient(coefficient, term):
        c = Expression.make_constant(constant)
        term.set_coefficient(c)
        return term


#############################

class Expression(object):
    def __init__(self, terms, var = "x"):
        assert isinstance(terms, list), "Terms must be a list"
        assert isinstance(var, str), "The Variable must be a string representing the Variable"
        # assert map(Term.isTerm, terms), "All Terms in an Expression must be of the Class Term"
        self.var = var
        self.termsList = terms

    @property
    def terms(self):
        return self.termsList

    def copy(self):
        return deepcopy(self)

    def _sort(self):
        # self.terms = sorted(self.terms, key = lambda x: x[1],reverse = True)
        return self

    # def combine_terms(self):
    #     combined = {}
    #     for term in self.terms:
    #         exp = term[1]
    #         coefficient = term[0]
    #         if combined.has_key(exp):
    #             combined[exp] = combined[exp] +  coefficient
    #         else:
    #             combined[exp] = coefficient
    #     self.terms = [(v, k) for k, v in combined.items()] # FLips Key Value Pairs
    #     return self

    def simplify(self):
        simplified = {}
        for term in self.terms:
            exp = term[1]
            coefficient = term[0]
            if coefficient == 0:
                continue
            else:
                simplified[exp] = coefficient
        self.terms = [(v, k) for k, v in simplified.items()] # FLips Key Value Pairs
        return self

    @staticmethod
    def add(e1, e2):
        assert map(Expression.isExpression, (e1, e2)), "Arguements must be of Class Term"
        assert e1.var == e2.var, "Single Variable Expressions Only"
        return Expression(e1.terms + e2.terms, e1.var )

    @staticmethod
    def subtract(e1,e2):
        # ERROR HANDLING DONE IN ADD
        return Expression.add(e1, Expression.negate(e2))

    #Done
    def negate(self):
        assert Expression.isExpression(self), "Arguements must be of Class Term"

        newTerms = []
        for term in self.terms:
            newTerms.append(Term.negate(term))
        self.termsList = newTerms
        return self

    def invert(self):
        e = Fraction._fractifyExpression(self)
        return e.invert()

    @staticmethod
    def multiply(e1, e2):
        assert map(Expression.isExpression, (e1, e2)), "Arguements must be of Class Term"
        assert e1.var == e2.var, "Single Variables Expressions Only"

        if Fraction.isFraction(e1) or Fraction.isFraction(e2):
            return Fraction.multiply(e1, e2)

        multiplied = []
        for term1 in e1.terms:
            for term2 in e2.terms:
                multiplied.append(Term.multiply(term1, term2))

        return Expression(multiplied)

    @staticmethod
    def divide(e1, e2):
        """ p1  divided by p2 -> p1/p2 """
        assert map(Expression.isExpression, (e1,e2)), ""
        return Expression.multiply(e1, e2.invert())

    @staticmethod
    def isZero(e):
        assert Expression.isExpression(e), "d"
        return False ##### FINISH

    @staticmethod
    def isExpression(e):
        return isinstance(e, Expression)

    @staticmethod
    def make_constant(c):
        return Expression([Term.make_constant(c)])

    def evaluate(self, x):
        return sum([term.evaluate(x) for term in self.termsList])

    # #Done
    # def __str__(self):
    #     output = ""
    #     for term in self.terms:
    #         c, exp = term
    #         if c == 0:
    #             continue
    #         elif exp == 0:
    #             output += str(c) + " + "
    #         elif c == 1:
    #             output +=  self.var + "^" + str(term[1]) + " + "
    #         else:
    #             output += str(term[0]) + self.var + "^" + str(term[1]) + " + "
    #     output = output[: len(output) - 3]
    #     return output



    def __str__(self):
        output = ""
        for term in self.terms:
            output += str(term) + " + "
        return output[:len(output) - 3]


class Fraction(Expression):

    def __init__(self, num, den, var = "x"):
        assert map(Expression.isExpression, (num, den))
        ## Asert is fraction
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
        return Fraction(e, Expression.make_constant(1))

    @staticmethod
    def add(f1, f2):
        assert isinstance(f1, Expression) and isinstance(f2, Expression)
        f1 = Fraction._fractifyExpression(f1)
        f2 = Fraction._fractifyExpression(f2)

        common_den = Expression.multiply(f1.den, f2.den)
        f1num = Expression.multiply(f1.num, f2.den)
        f2num = Expression.multiply(f2.num, f1.den)
        return Fraction(Expression.add(f1num, f2num), common_den)

    @staticmethod
    def multiply(f1,f2):
        assert isinstance(f1, Expression) and isinstance(f2, Expression)
        f1 = Fraction._fractifyExpression(f1)
        f2 = Fraction._fractifyExpression(f2)
        return Fraction(Expression.multiply(f1.num, f2.num), Expression.multiply(f1.den, f2.den))

    @staticmethod
    def divide(f1,f2):
        assert isinstance(f1, Fraction) and isinstance(f2, Fraction)
        return Expression.multiply(f1, f2.copy().invert())

    def invert(self):
        self.num, self.den = self.den, self.num
        return self

    def reduceCoefficients(self):
        coefficients = list(map(Term.get_coefficient, self.num.terms)) + list(map(Term.get_coefficient, self.den.terms))
        def gcd_help(x,y):
            x,y = map(abs, (x,y))
            if y > x:
                return gcd_help(y,x)
            if y == 0:
                return x
            return gcd_help(y, x % y)
        gcd = reduce(gcd_help, coefficients)
        for x in self.num.terms:
            x.set_coefficient(x.get_coefficient()/gcd)
        for x in self.den.terms:
            x.set_coefficient(x.get_coefficient()/gcd)
        return self

    def evaluate(self, x):
        return self.num.evaluate(x) / self.den.evaluate(x)

    @staticmethod
    def isFraction(f):
        return isinstance(f, Fraction)

    def __str__(self):
        print(self.num)
        if str(self.den) == "1.0" or str(self.den) == "1":
            return str(self.num)
        return "(" + str(self.num) + ")/(" + str(self.den) + ")"

##############################

class Term(Expression):
    def __init__(self, var = "x"):
        self.var = var

    def __eq__(self, t) :
            return self.__dict__ == t.__dict__ and self.__class__ == t.__class__

    @property
    def terms(self):
        return [self]

    def copy(self):
        return deepcopy(self)

    def get_coefficient(self):
        return self.c

    def set_coefficient(self, coefficient):
        self.c = coefficient
        return self

    @staticmethod
    def make_constant(c):
        return PowerTerm(c, X(), 0)

    @staticmethod
    def add(t1, t2):
        assert map(Term.isTerm, (t1,t2)), "Arguements must be of type ExponentialTerm"
        if type(t1) == type (t2):
            return type(t1).add(t1,t2)
        return Expression([t1, t2])

    @staticmethod
    def subtract(t1, t2):
        assert map(Term.isTerm, (t1,t2)), "Arguements must be of type ExponentialTerm"
        return Term.add(t1, Term.negate(t2))

    @staticmethod
    def multiply(t1, t2):
        assert map(Term.isTerm, (t1,t2)), "Arguements must be of type ExponentialTerm"
        if t1.__class__ == t2.__class__:
            return t1.__class__.multiply(t1,t2)
        return Expression([MultTerm(t1,t2)])

    @staticmethod
    def negate(t1):
        return type(t1).negate(t1)


    def evaluate(self, x):
        return self.evaluate(x)

    @staticmethod
    def isTerm(t):
        return isinstance(t, Term)

class X(Term):
    def __init__(self, var = "x"):
        super(X, self).__init__( var)

    @property
    def terms(self):
        return [self]

    def evaluate(x):
        return x

    def __str__(self):
        return str(self.var)

class MultTerm(Term):
    def __init__(self, terms, var = "x"):
        super(MultTerm, self).__init__(var)
        self.termsMultiplied = terms

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
    def __init__(self, coefficient = 1, main = X(), exp = 1, var = "x"):
        super(PowerTerm, self).__init__(var)
        assert isinstance(exp, numbersModule.Number)
        assert isinstance(main, Expression)
        self.c = coefficient
        self.main = main
        self.exp = exp

    def add(p1, p2):
        assert map(PowerTerm.isPwrTerm, (p1,p2)), "Arguements must be of type ExponentialTerm"
        if p1.exp == p2.exp and p1.main == p2.main:
            return PowerTerm(p1.c + p2.c, p1.main, p1.exp)
        else:
            return Expression([p1,p2])

    def multiply(p1, p2):
        assert map(PowerTerm.isPwrTerm, (p1,p2)), "Arguements must be of type ExponentialTerm"
        if p1.exp == p2.exp and p1.main == p2.main:
            return PowerTerm(p1.c * p2.c, p1.main, p1.exp + p2.exp)
        return MultTerm([p1,p2])

    def negate(p1):
        return PowerTerm(-p1.c, p1.exp)

    def evaluate(self, x):
        return self.c * pow(x, self.exp)

    def isPwrTerm(p):
        return isinstance(p, PowerTerm)

    def __str__(self):
        if self.exp == 0:
            return str(self.c)
        if self.c == 0:
            return "0"
        else:
            return str(self.c) + self.var + "^" + str(self.exp)

class ExponentialTerm(Term):
    def __init__(self, coefficient = 1, exp = 1, var = "x"):
        super(ExponentialTerm, self).__init__(var)
        assert isinstance(self.exp, Expression)
        self.c = coefficient
        self.exp = exp

    def add(e1, e2):
        assert map(ExponentialTerm.isExpTerm, (e1,e2)), "Arguements must be of type ExponentialTerm"
        if e1.exp == e2.exp:
            return ExponentialTerm(e1.c + e2.c, e1.exp)
        else:
            return Expression([e1,e2])

    def multiply(e1, e2):
        assert map(ExponentialTerm.isExpTerm, (e1,e2)), "Arguements must be of type ExponentialTerm"
        return ExponentialTerm(e1.c * e2.c, e1.exp + e2.exp)

    def negate(e1):
        return ExponentialTerm(-e1.c, e1.exp)

    def evaluate(self, x):
        return self.c * math.exp(self.exp.evaluate(x))

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


    def add(l1, l2):
        assert map(LogTerm.isExpTerm, (e1,e2)), "Arguements must be of type ExponentialTerm"
        # if l1.base == l2.base:
        #     return LogTerm(e1.c + e2.c, e1.exp)
        # else:
        return Expression([e1,e2])

    def multiply(e1, e2):
        return MultTerm((e1,e2))

    def negate(l1):
        return LogTerm(-l1.c, l1.base, l1.beta)

    def evaluate(self, x):
        return self.c * math.log(self.insideTerm.evaluate(x), self.base)

    def isLogTerm(e):
        return isinstance(e, LogTerm)

    def __str__(self):
        base = "e" if self.base == math.e else str(self.base)
        return str(self.c) + "log_" + base + "(" + str(self.insideTerm) + ")"

###############################

###???###
class Numbers(object):
    pass

class Reals(Numbers):
    pass

class Rationals(Reals):
    def __init__(self, *args):
        # assert map(lambda x: )
        if len(args) == 1:
            self.num = args[0]
            self.den = 1
        if len(args) == 2:
            self.num = args[0]
            self.den = args[1]

class Irationals(Reals):
    pass

class Integer(Rationals):
    pass
