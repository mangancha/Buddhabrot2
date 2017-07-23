#include <Rcpp.h>
#include "random.cpp"
using namespace std;

/*
in_set function taken from:
XANDER'S BUDDHABROT PROGRAM OF DOOM
REVISION DATE:  26 March 2012
DETERMINES WHETHER A GIVEN POINT IN THE COMPLEX PLANE IS IN THE dTH DEGREE
MULTIBROT SET, AND RETURNS A BOOLEAN. 

I've included optimization tests to avoid points on main cardioid, and main bulbs
Also I've added a periodicity check
*/

bool in_set(double c[2], int d, int numIterations, int bailout) {

	int iteration = 1;
	bool inSet = true;
	double z[2];
	double xtemp, ytemp;
	
	z[0]=0;
	z[1]=0;
	
	if(((c[0]*c[0])+(c[1]*c[1])) > bailout) {

                return false;

	}

	
	// period-2 bulb test
	if ((((c[0]+1.0f)*(c[0]+1.0f)) + c[1]*c[1]) < 0.0625f)
		return true;
	
	// cardioid test 
   	float q = (c[0]-0.25f)*(c[0]-0.25f) + c[1]*c[1];
    	if (q * ( q + (c[0]-0.25f)) < 0.25f*c[1]*c[1] )
		return true;	

	
	// test for the smaller bulb left of the period-2 bulb
        if ((((c[0]+1.309f)*(c[0]+1.309f)) + c[1]*c[1]) < 0.00345f)
		return true;

	// check for the smaller bulbs on top and bottom of the brot
    	if ((((c[0]+0.125f)*(c[0]+0.125f)) + (c[1]-0.744f)*(c[1]-0.744f)) < 0.0088f)
		return true;
    	if ((((c[0]+0.125f)*(c[0]+0.125f)) + (c[1]+0.744f)*(c[1]+0.744f)) < 0.0088f)
		return true;
	

	while(inSet && iteration < numIterations) {
		xtemp = z[0]*z[0] - z[1]*z[1] + c[0];
		ytemp = z[0]*z[1] + z[0]*z[1] + c[1];
		++iteration;

		// periodicity check	
		if(z[0] == xtemp && z[1] == ytemp)
			return true;

		z[0] = xtemp;
		z[1] = ytemp;

		if(((z[0]*z[0])+(z[1]*z[1])) > bailout) {
			return false;
		}

		
	}
	
}

using namespace Rcpp;

RcppExport SEXP buddhabrot(SEXP nx_, SEXP ny_,  SEXP bailout_, SEXP iter_, SEXP keep_, SEXP batchsize_, SEXP power_, SEXP seed_)
{
	int nx = as<int>(nx_), ny = as<int>(ny_), bailout = as<int>(bailout_), iter = as<int>(iter_), keep = as<int>(keep_), batchsize = as<int>(batchsize_), power = as<int>(power_), s = as<int>(seed_);
	
	/*theset is a vector containing the values of the image. It is transformed into a matrix by R*/
	NumericVector theset(nx*ny);
	int d = power;
	int bail = bailout;
    unsigned long seed = s;
	int maxIter = iter;
	float ptkeep = keep;
	float cutoff = (maxIter*((100-ptkeep)/100));
	int pixelX, pixelY;
	long int numPoints = batchsize;
	double c[2], z[2];
	double t;
	bool inSet;

	init_genrand(seed);
	
	for(long int i=0; i<numPoints;) 
	{
		// SELECT A RANDOM POINT IN THE COMPLEX PLANE
		c[0] = random(-2,2);
		c[1] = random(-2,2);

		//This is needed to avoid having thousands/millions of z point mapping to 0,0
		z[0] = c[0];
		z[1] = c[1];		



		inSet = in_set(c,d,maxIter,bail);

		// IF SO, INCREMENT THE PIXELS IT PASSES THROUGH
		if(!inSet) {

			for(int j=1; j<=maxIter; ++j) 
			{
				pixelX = nx * ((z[0] + 2.0)/4.0);
				pixelY = ny * ((z[1] + 2.0)/4.0);
				if(pixelX > 0 && pixelX < nx && pixelY < ny && pixelY > 0) 
					{

					if(j > cutoff) {
						theset[(pixelX * (ny)) + pixelY]++;
						
					}
						t = z[0]*z[0] - z[1]*z[1] + c[0];
						z[1] = z[0]*z[1] + z[0]*z[1] + c[1];
						z[0] = t;

				} else {
					j = maxIter + 1;
				}
			}
			++i;
		}		
	}
	return wrap(theset);
}

