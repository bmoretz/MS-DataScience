from sympy import symbols, integrate, Rational, lambdify, sqrt
import matplotlib.pyplot as plt
import numpy as np

# For a certain​ drug, the rate of reaction in appropriate units is given by

t = symbols( 't', positive = True )
dR =  ( 2 / ( t + 1 ) ) + ( 2 / sqrt( t + 1 ) )

# where t is time​ (in hours) after the drug is administered.  Find the total reaction to the drug over the following time periods.

# The total reaction from 1-9 is:
t_1 = integrate( dR, ( t, 1, 9 ) ).evalf()
round( t_1, 3 )

# The total reaction from 9-24 is:
t_2 = integrate( dR, ( t, 9, 24 ) ).evalf()
round( t_2, 3 )

g_xlim = [ 1, 30 ]
g_ylim = [ -5, 15 ]

lam_p = lambdify( t, integrate( dR, t ), np )

x_vals = np.linspace( g_xlim[0], g_xlim[1], 1000, endpoint=True )
y_vals = lam_p( x_vals )
plt.plot( x_vals, y_vals )
plt.show()