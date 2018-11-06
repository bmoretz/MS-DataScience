# Suppose the total cost​ C(x) (in​ dollars) to manufacture a quantity x of weed killer​ (in hundreds of​ liters) is given by the function
# C(x) = x^3 - 3*x^2 + 8*x + 50

# Where is​ C(x) decreasing?
	# It Doesn't.

# Where is​ C(x) increasing? 
	# -Inf, Inf

from sympy import *
from sympy.plotting import (plot, plot_parametric,
                            plot3d_parametric_surface, plot3d_parametric_line,
                            plot3d)

init_printing()

def disp_fun( f ):
	pprint( '\n{0}\n\n'.format( pretty( f ) ) )

x = symbols( 'x' )

C = x**3 - 3*x**2 + 8*x + 50
dC = diff( C, x )

dis = discriminant( dC )

p = plot( C, line_color = 'B', show = False )

if dis < 0:
	p2 = plot( dC, line_color = 'R', title = 'Continuous', show = False )
else:
	critical_numbers = solve( dC, x )

p.legend = True
p.append( p2[ 0 ] )
p.show()