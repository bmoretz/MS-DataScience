# Graphs of the cost​ C(x), revenue​ R(x) and the profit​ P(x), in thousands of​ dollars, are​ shown, where x is the number of thousands of items produced.

from sympy import solve, symbols, lambdify, diff
import matplotlib.pyplot as plt
import numpy as np

x1, y1 = symbols( 'x1, y1', positive = True )

g_xlim = [ 1, 13 ]
g_ylim = [ -5, 15 ]

def plot_fun( fun, name, col ):
	x_vals = np.linspace( g_xlim[0], g_xlim[1], 1000, endpoint=True )
	y_vals = fun( x_vals )
	plt.plot( x_vals, y_vals, label = name, color = col )

x = symbols( 'x' )
R = 100*x
P = -x**3 + 15*x**2 - 27*x - 50

# Revenue
lam_R = lambdify( x, R, np )
plot_fun( lam_R, 'R', 'R' )

# Profit
lam_P = lambdify( x, P, np )
plot_fun( lam_P, 'P', 'Black' )

# The profit is given by-x**3 + 15*x**2 - 27*x - 50. What is the formula for the cost function​ C(x)?
# Cost Function
C = R - P
lam_C = lambdify( x, C, np )
plot_fun( lam_C, 'C', 'Blue' )

# Fixed Cost
C.subs( { x: 0 } )

# Average Cost
avg_cost = C / x
lam_ac = lambdify( x, avg_cost, np )
plot_fun( lam_ac, 'AC', 'Orange' )

margin_cost = diff( C, x )
lam_mc = lambdify( x, margin_cost, np )
plot_fun( lam_mc, 'M', 'G' )

# Report the minimum marginal cost.
min_cost = solve( diff( margin_cost, x ), x )[ 0 ]
margin_cost.subs( { x: min_cost} )

# What is the largest profit​ possible?
profit = diff( P, x )
solve( profit, x )

P.subs( { x: 9 } )

plt.show()