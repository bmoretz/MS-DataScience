# Find the point of diminishing returns (x,y) for the function​ R(x),
# where​ R(x) represents revenue​ (in thousands of​ dollars) and x represents the amount spent on
# advertising​ (in thousands of​ dollars).

# R(x) = 11,000 - x^3 + 42x^2 + 800x,
#  0 <= x <= 20

from sympy import solve, Limit, lambdify, symbols, diff, ln, discriminant
import matplotlib.pyplot as plt
import numpy as np

domain_end = 20

g_xlim = [-5, 40]
g_ylim = [-5, 70]

# eq

x = symbols('x', positive = True ) # 0 <= x
R = 11000 - x ** 3 + 42 * x ** 2 + 800 * x # Revenue

lam_x = lambdify( x, R, np )

x_vals = np.linspace( g_xlim[0], g_xlim[1], 1000, endpoint=True )
y_vals = lam_x( x_vals )

plt.plot( x_vals, y_vals, label = 'Revenue' )

# Inflection Point, second derivative, ( Green )

dR = diff( R, x, 2 )
inflection_point = solve( dR, x )[ 0 ]
inflection_value = R.subs( { x: inflection_point } )

section = np.arange( 0, int( inflection_point ), 1/20.)
plt.fill_between( section,lam_x(section), alpha = .4 )

plt.vlines( x = inflection_point, ymin = 0, ymax = inflection_value, color='G', zorder = 1 )
plt.hlines( y = inflection_value, xmin = 0, xmax = inflection_point, color = 'G', zorder = 1 )
plt.text( 0, inflection_value + 2000, 'Inflection Point: {0}'.format( inflection_value ) )

# Domain End (Black)
plt.vlines( x = domain_end, ymin = 0, ymax = R.subs( { x: domain_end } ), color='Black', zorder=2)
plt.text( domain_end + 1, 1000, 'Domain Bounds' )

# Linear Domain

plt.xlabel('ROI')
plt.ylabel('Advertising $(k)')
plt.legend()
plt.show()