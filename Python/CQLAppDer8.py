# A contagious and fatal virus has tragically struck the city of​ Plaguesville, which has been quarantined until the virus has run its course. The function 
# P(t) gives the number of people who are newly infected t days after the outbreak began. 
#
# Find first and second derivatives P'(t) and P''(t)
# Solve P(t) = 0, P(t) and P''(t) = 0, then complete the table.
# Use the table to describe the details of the Plagesville tragedy.

from sympy import solve, symbols, lambdify, diff
import matplotlib.pyplot as plt
import numpy as np

g_xlim = [ 1, 100 ]
g_ylim = [ -5, 15 ]

def plot_fun( fun, name, col ):
	x_vals = np.linspace( g_xlim[0], g_xlim[1], 1000, endpoint=True )
	y_vals = fun( x_vals )
	plt.plot( x_vals, y_vals, label = name, color = col )

t = symbols( 't' )
P = 6.4*t**3 - 0.064* t**4

dP = diff( P, t )
ddP = diff( dP, t )

lam_P = lambdify( t, P )
plot_fun( lam_P, 'P', 'B' )

lam_dP = lambdify( t, dP )
plot_fun( lam_dP, 'P\'', 'G' )

lam_ddP = lambdify( t, ddP )
plot_fun( lam_ddP, 'P\'\'', 'R' )

plt.show()

solve( P, t )
solve( dP, t )
solve( ddP, t )

P.subs( { t: 0 } )
dP.subs( { t: 75 } )
ddP.subs( { t: 0 } )
