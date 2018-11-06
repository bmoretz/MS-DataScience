from sympy import symbols, integrate, Rational, lambdify, sqrt, solve
import matplotlib.pyplot as plt
import numpy as np

def plot_fun( fun, name, col ):
	x_vals = np.linspace( g_xlim[0], g_xlim[1], 1000, endpoint=True )
	y_vals = fun( x_vals )
	plt.plot( x_vals, y_vals, label = name, color = col )

# Suppose a company wants to introduce a new machine that will produce a rate of annual savings​ (in dollars) given by the function Upper S'(x)
# where x is the number of years of operation of the​ machine, while producing a rate of annual costs​ (in dollars) given by the function C'(x)
x = symbols( 'x', positive = True )

dS = 212 - x**2
dC = x**2 + Rational( 6, 5 )*x

P = dS - dC

solve( P, x )

# What are the net total savings during the first year of use of the​ machine?
savings = integrate( P, ( x, 0, 1 ) ).evalf()
round( savings )

#  What are the net total savings over the entire period of use of the​ machine
savings = integrate( P, ( x, 0, 10 ) ).evalf()
round( savings )

g_xlim = [ 1, 30 ]
g_ylim = [ -5, 15 ]

lam_s = lambdify( x, integrate( dS, x ), np )
plot_fun( lam_s, 'S', 'B' )

lam_c = lambdify( x, integrate( dC, x ), np )
plot_fun( lam_c, 'C', 'Orange' )

lam_p = lambdify( x, integrate( P, x ), np )
plot_fun( lam_p, 'P', 'G' )

plt.legend()
plt.show()
