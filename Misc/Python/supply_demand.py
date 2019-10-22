from sympy import symbols, integrate, Rational, lambdify, sqrt, solve
import matplotlib.pyplot as plt
import numpy as np

def plot_fun( fun, name, col ):
	x_vals = np.linspace( g_xlim[0], g_xlim[1], 1000, endpoint=True )
	y_vals = fun( x_vals )
	plt.plot( x_vals, y_vals, label = name, color = col )

g_xlim = [ 1, 50 ]
g_ylim = [ -5, 15 ]

q = symbols( 'q', positive = True )

# The supply function for oil is given​ (in dollars) by
S = q**2 + 11*q

# and the demand function is given​ (in dollars) by:
D = 1054 - 17*q - q**2

# Find the point at which supply and demand are in equilibrium.
E = S - D

equilibrium_x = solve( E, q )[ 0 ]
equilibrium_y = S.subs( { q: equilibrium_x } )

# Find the​ consumers' surplus.

CS = D - equilibrium_y
consumer_surplus = integrate( CS, ( q, 0, equilibrium_x ) ).evalf()
round( consumer_surplus, 2 )

# Find the​ consumers' surplus.

SS = equilibrium_y - S
suplier_surplus = integrate( SS, ( q, 0, equilibrium_x ) ).evalf()
round( suplier_surplus, 2 )

# Graph
lam_s = lambdify( q, S, np )
plot_fun( lam_s, 'Supply', 'B' )

lam_d = lambdify( q, D, np )
plot_fun( lam_d, 'Demand', 'R' )

plt.legend()
plt.show()
