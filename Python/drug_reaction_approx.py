from sympy import *
import matplotlib.pyplot as plt
import numpy as np

# The reaction rate to a new drug is given by the following function where t is time​ (in hours) after the drug is administered.

t = symbols( 't', positive = True )
S = exp( -t**2 ) + 3 / ( t + 3 )
a, b, n = 1, 9, 8

def trapezoidal_rule( f, a, b, n ):
	area = 0

	steps = np.linspace( a, b, n + 1, endpoint = True )

	for i in range( 0, n + 1 ):

		v = round( f.subs( { t: steps[ i ] } ), 4 )

		if i == 0 or i == n:
			area += .5 * v
		else:
			area += v
	
	return area * ( b - a ) / n

def simpsons_rule( f, a, b, n ):
	area = 0

	steps = np.linspace( a, b, n + 1, endpoint = True )

	for i in range( 0, n + 1 ):

		v = round( f.subs( { t: steps[ i ] } ), 4 )

		if i == 0 or i == n:
			area += v
		elif i % 2 == 0:
			area += 2 * v
		else:
			area += 4 * v

	return area * ( b - a ) / ( 3*n )

area = simpsons_rule( S, a, b, n )

g_xlim = [ 0, 10 ]

lam_s = lambdify( t, S, np )
x_vals = np.linspace( g_xlim[0], g_xlim[1], 1000, endpoint = True )
y_vals = lam_s( x_vals )

plt.vlines( x = a, ymin = 0, ymax = S.subs( { t: a } ), color = 'Black', zorder = 1, alpha = .4 )
plt.vlines( x = b, ymin = 0, ymax = S.subs( { t: b } ), color = 'Black', zorder = 1, alpha = .4 )

bounds = np.arange( a, b, 1/25., dtype=float)

for n in bounds:
	y = S.subs( { t: n } )
	plt.vlines( x = n, ymin = 0, ymax = y, color = 'Teal', zorder = 1, alpha = .2 )

plt.title( 'Simpson''s Rule Area: {0}'.format( round( area, 3 ) ) )
plt.plot( x_vals, y_vals )
plt.show()