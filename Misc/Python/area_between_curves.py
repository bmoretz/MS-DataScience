from sympy import symbols, integrate, Rational, lambdify, solve
import matplotlib.pyplot as plt
import numpy as np

g_xlim = [ -8, 8 ]

def plot_fun( fun, name, col ):
	x_vals = np.linspace( g_xlim[0], g_xlim[1], 1000, endpoint=True )
	y_vals = fun( x_vals )
	plt.plot( x_vals, y_vals, label = name, color = col )

a, b = 4, 6
x = symbols( 'x' )
F = x**4 + x**3 + x**2 + 4
G = -5*x**3 + x**2 + 1

lam_f = lambdify( x, F, np )
plot_fun( lam_f, str(F), 'B' )

lam_g = lambdify( x, G, np )
plot_fun( lam_g, str(G), 'R' )

H = F - G

plt.vlines( x = a, ymin = G.subs( { x: a } ), ymax = F.subs( { x: a } ), color = 'Black', zorder = 1, alpha = .4 )
plt.vlines( x = b, ymin = G.subs( { x: b } ), ymax = F.subs( { x: b } ), color = 'Black', zorder = 1, alpha = .4 )

bounds = np.arange( a, b, 1/500., dtype=float)

for n in bounds:
	y1 = F.subs( { x: n } )
	y2 = G.subs( { x: n } )
	plt.vlines( x = n, ymin = y2, ymax = y1, color = 'Teal', zorder = 1, alpha = .2 )

area = integrate( H, ( x, a, b ) ).evalf()

plt.title( 'Area Between a [{0}] and b [{1}] is {2}'.format( a, b, round( area, 3 ) ) ) 
plt.legend()
plt.show()