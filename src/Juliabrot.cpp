#include <Rcpp.h>
#include "random.cpp"

using namespace Rcpp;

RcppExport SEXP juliabrot(SEXP nx_, SEXP ny_,  SEXP bailout_, SEXP iter_, SEXP keep_, SEXP batchsize_, SEXP power_, SEXP seed_, SEXP re_, SEXP im_)
{
	int nx = as<int>(nx_), ny = as<int>(ny_), bailout = as<int>(bailout_), iter = as<int>(iter_), keep = as<int>(keep_), batchsize = as<int>(batchsize_), power = as<int>(power_), s = as<int>(seed_);
	double re = as<double>(re_), im = as<double>(im_);
	
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
		// SELECTED (FIXED) C POINTS
		c[0] = re;
		c[1] = im;
		
		// GET 2 RANDOM Z POINTS
		z[0] = random(-2,2);
		z[1] = random(-2,2);


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
	return wrap(theset);
}

