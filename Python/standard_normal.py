# The standard normal probability function is used to describe many different populations.
# f(x) = 1 / sqrt( 3*pi ) e ^ -x^2/2

from sympy import *
import matplotlib.pyplot as plt
import numpy as np

g_xlim = [ -4, 4 ]
g_ylim = [ 0, .5 ]
g_title = 'Standard Normal'

a, b = 1, 2

x = symbols( 'x' )

F = ( 1 / ( 3*pi ) ** .5 ) * exp( ( -x**2 / 2 ) )
dF = diff( F, x )

lam_f = lambdify( x, F, np )
x_vals = np.linspace( g_xlim[0], g_xlim[1], 1000, endpoint=True )
y_vals = lam_f( x_vals )

plt.plot( x_vals, y_vals, color = 'B' )

plt.vlines( x = a, ymin = 0, ymax = F.subs( { x: a } ), color = 'Black', zorder = 1, alpha = .4 )
plt.vlines( x = b, ymin = 0, ymax = F.subs( { x: b } ), color = 'Black', zorder = 1, alpha = .4 )

plt.hlines( y = 0, xmin = g_xlim[0], xmax = g_xlim[1], color = 'Black', zorder = 1 )

bounds = np.arange( a, b, 1/250., dtype=float)

for n in bounds:
	y = F.subs( { x: n } )
	plt.vlines( x = n, ymin = 0, ymax = y, color = 'Teal', zorder = 1, alpha = .4 )

area = integrate( F, ( x, a, b ) ).evalf()
total_area = integrate( F, ( x, g_xlim[0], g_xlim[1] ) ).evalf()

area_pct = round( ( area / total_area ) * 100 )
plt.text( 2 , .1, 'Area: {0}%'.format( area_pct ) )

plt.title( g_title )
plt.legend = True
plt.show()