from sympy import symbols, integrate, Rational, lambdify
import matplotlib.pyplot as plt
import numpy as np

# Pollution from a factory is entering a lake. The rate of concentration of the pollutant at time t is given by
t = symbols( 't', positive = True )
dP = 91*t ** Rational( 5, 2 )

# where t is the number of years since the factory started introducing pollutants into the lake.

#  Ecologists estimate that the lake can accept a total level of pollution of 7600 units before all the fish life in the lake ends. 
#  Can the factory operate for 5 years without killing all the fish in the​ lake?

# Yes, because:
P = integrate( dP, ( t, 0, 5 ) ).evalf()
round( P )
# is less than 7600.

# What is the polution doing?
g_xlim = [ 1, 10 ]
g_ylim = [ -5, 15 ]

lam_p = lambdify( t, integrate( dP, t ), np )

x_vals = np.linspace( g_xlim[0], g_xlim[1], 1000, endpoint=True )
y_vals = lam_p( x_vals )
plt.plot( x_vals, y_vals )
plt.show()