# A piece of wire 17 ft. long is cut into two pieces. One piece is made into a circle and the other piece is made into a square. Let the piece of length x be formed into a circle.
# 
# Allow x to equal 0 or 17​, so all the wire is used for the square or the circle. 
# Where should the cut be made to make the sum of the areas​ maximum? 
# (Hint: Remember to use the endpoints of a domain when looking for absolute maxima and​ minima.)

import math
from sympy import Derivative, symbols, solve, sympify, pi
from collections import namedtuple

Pieces = namedtuple( 'Pieces', [ 'circumference', 'side'] )

def cut_wire( l ):
	# Side, Circumference
	s, c = symbols( 's, c' )
	# V = s^2 ( square ) + pi * ( c / 2pi )^2
	A = ( s / 4 ) ** 2 + c**2 / ( 4 * math.pi )
	
	# c + s = l
	L = c + s - l

	# Solve for c in terms of s
	sC = solve( L, s )[ 0 ]
	
	# A1 = area in terms of a single variable
	A1 = A.subs( s, sC )
	
	# Now single variable, take The derivitave with respect to c.
	dA = Derivative( A1, c ).doit()
	slns = solve( dA )

	min_c = slns[ 0 ]
	min_s = l - min_c

	return Pieces( circumference = min_c, side = min_s )

def area_sqare( s ):
	return s**2

def area_circle( c ):
	return c**2 / ( 4 * math.pi )

if __name__ == '__main__':

	try:
		l = float( input( 'Enter the length of the wire to cut: ') )
	except ValueError:
		print( 'Error parsing values' )
	else:
		print( 'wire length: {0}\n'.format( l ) )

		pieces = cut_wire( l )
		areaCircle = area_circle( pieces.circumference )
		areaSqare = area_sqare( pieces.side )
		
		print( 'Mimimum Area: {0}'.format( areaCircle + areaSqare ) )
		print( 'Circle Area: {0}'.format( areaCircle ) )
		print( 'Square Area: {0}'.format( areaSqare ) )

		print( 'Maximum Area: Circle = {0} ({1} len), Square = 0, (0 len)'.format( area_circle( l ), l ) )
