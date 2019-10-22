from sympy import *
import matplotlib.pyplot as plt
import numpy as np

# The results from a research study in psychology are shown in the chart. Complete parts​ (a) and​ (b) below.

points = [ [ 1, 2 ], [ 2, 3 ], [ 3, 11 ], [ 4, 10 ], [ 5, 13 ], [ 6, 18], [ 7, 24] ]

def trapezoidal_rule( V, a, b, n ):
	area = 0

	steps = np.linspace( a, b, n + 1, endpoint = True )

	for i in range( 0, n + 1 ):

		v = V[ i ]

		if i == 0 or i == n:
			area += .5 * v
		else:
			area += v
	
	return area * ( b - a ) / n

def simpsons_rule( V, a, b, n ):
	area = 0

	steps = np.linspace( a, b, n + 1, endpoint = True )

	for i in range( 0, n + 1 ):

		v = V[ i ]

		if i == 0 or i == n:
			area += v
		elif i % 2 == 0:
			area += 2 * v
		else:
			area += 4 * v

	return area * ( b - a ) / ( 3*n )

p_x = []
p_y = []

for p in points:
	p_x.append( p[ 0 ] )
	p_y.append( p[ 1 ] )

a, b, n = min( p_x ), max( p_x ), len( p_x ) - 1

area = simpsons_rule( p_y, a, b, n )
round( area, 3 )

plt.plot( p_x, p_y, color = 'black', marker = 'o' )

plt.vlines( x = a, ymin = 0, ymax = p_y[a - 1], color = 'Red' )
plt.vlines( x = b, ymin = 0, ymax = p_y[b - 1], color = 'Red' )

plt.show()