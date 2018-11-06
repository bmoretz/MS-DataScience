from sympy import ( symbols, solve, diff, integrate, exp, sqrt, lambdify, pprint )
import matplotlib.pyplot as plt
import numpy as np

# The annual rainfall in a remote country varies from 0 to 5 inches and is a random variable with probability density function defined by:

x = symbols( 'x' )
F = ( 5.5 - x ) / 15

# the following probabilities for the annual rainfall in a randomly selected year.

# What does the petal distribution look like?

g_xlim = [ -1, 9 ]

lam_p = lambdify( x, F, np )
x_vals = np.linspace( g_xlim[0], g_xlim[1], 1000, endpoint=True )
y_vals = lam_p( x_vals )
plt.plot( x_vals, y_vals )

x_min, x_max = 0, 5

plt.hlines( y = 0, xmin = g_xlim[0], xmax = g_xlim[1], color = 'Black', zorder = 1 )

plt.vlines( x = x_min, ymin = 0, ymax = F.subs( { x: x_min } ), color = 'Black', zorder = 1 )
plt.vlines( x = x_max, ymin = 0, ymax = F.subs( { x: x_max } ), color = 'Red', zorder = 1 )

# The probability that the annual rainfall is between 1 and 2 inches.

a, b = 1, 2
bounds = np.arange( a, b, 1/50., dtype=float)

for n in bounds:
	y = F.subs( { x: n } )
	plt.vlines( x = n, ymin = 0, ymax = y, color = 'Teal', zorder = 1, alpha = .4 )

area = integrate( F, ( x, a, b ) ).evalf()
total_area = integrate( F, ( x, 0, x_max ) ).evalf()

area_pct = round( ( area / total_area ), 4 )

plt.text( 2 , .1, 'Area: {0}'.format( area_pct ) )
plt.show()

# The probability that the annual rainfall is less than 3 inches.

a, b = 0, 3
bounds = np.arange( a, b, 1/50., dtype=float)

for n in bounds:
	y = F.subs( { x: n } )
	plt.vlines( x = n, ymin = 0, ymax = y, color = 'Teal', zorder = 1, alpha = .4 )

area = integrate( F, ( x, a, b ) ).evalf()
total_area = integrate( F, ( x, 0, x_max ) ).evalf()

area_pct = round( ( area / total_area ), 4 )

plt.text( 2 , .1, 'Area: {0}'.format( area_pct ) )
plt.show()

# The probability that the annual rainfall is greater than 4 inches.

a, b = 4, x_max
bounds = np.arange( a, b, 1/50., dtype=float)

for n in bounds:
	y = F.subs( { x: n } )
	plt.vlines( x = n, ymin = 0, ymax = y, color = 'Teal', zorder = 1, alpha = .4 )

area = integrate( F, ( x, a, b ) ).evalf()
total_area = integrate( F, ( x, 0, x_max ) ).evalf()

area_pct = round( ( area / total_area ), 4 )

plt.text( 2 , .1, 'Area: {0}'.format( area_pct ) )
plt.show()

