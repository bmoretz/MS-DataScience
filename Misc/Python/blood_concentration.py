# The percent of concentration of a certain drug in the bloodstream x hours after the drug is administered is given by 
# K(x) = 3x / x^2 + 4

from sympy import solve, Limit, lambdify, symbols, diff, ln, discriminant
import matplotlib.pyplot as plt
import numpy as np

x = symbols('x', positive = True ) # 0 <= x
K = 3*x / ( x**2 + 4 )

domain_end = 20

g_xlim = [-5, 40]
g_ylim = [-5, 70]

lam_x = lambdify( x, K, np )

x_vals = np.linspace( g_xlim[0], g_xlim[1], 1000, endpoint=True )
y_vals = lam_x( x_vals )

plt.plot( x_vals, y_vals, label = '' )

# Find the time at which the concentration is a maximum.

dK = diff( K, x, 1 )
max = solve( dK, x )[ 0 ].evalf()

# The maximum concentration is:
maxPct = round( K.subs( { x: max} ), 2 )

plt.scatter( max, maxPct, color = 'R' )

plt.show()