# PyExpressions Library

by Alvin Wan and Nathan Pucheril

## Getting Started

To use, install via PyPi.

```
pip install PyExpressions
```

To use, import needed scripts and see below for the appropriate section.

```
import pyexpressions

# or
from pyexpressions import Polynomial
```

## Discrete Mathametics

This library supports the following methods.

- Lagrange Interpolation: `PyExpressions.lagrangeinterpolation.interpolate`
- Lagrange Interpolation in a Galois Field: TODO

## Polynomial

The polynomial (will eventually be) is designed to be type-agnostic.
Coefficients can be any numerical type, even custom-made.

Polynomials accept human-readable input and evaluate to human-readable output.

```
>>> Polynomial('3x^6 + 2x^2 + x + 1')
3x^6 + 2x^2 + x + 1
```

In addition, this `Polynomial` class supports standard mathematical operations,
that can be mixed with other numerical types.

```
>>> p, q, r = P('1'), P('2x + 3'), ('3x^4 + 2x^2 + 1')
>>> p + q
2x + 4
>>> r * 4
12x^4 + 8x^2 + 4
```

The polynomial can then be evaluated at various points, as you would expect a
`Polynomial` to do.

```
>>> x2 = P('x^2')
>>> x2(-1)
1
>>> x2(3)
9
```
