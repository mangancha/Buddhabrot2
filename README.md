Buddhabrot2
===========
This project includes.

* R scripts to render a Buddhabrot (and a Juliabrot) with support for parallel procesing.
* Shared objects contained in the lib folder, precompiled for 4 platforms (WInx64, Linux, MacOs and Arm7 (Raspberry Pi)). These shared objects are called by the R scripts.
* The C++ code needed to compile those shared objects using Rcpp (buddhabrot.cpp, xander.cpp, random.cpp) is included in src.

How to use:
* Download to a directory of your choosing. 
* Open buddhabrot.R and set the 'OS' variable to your operating system. 
* Also set the 'Save.Dir' variable to you choosing. You'll be able to save your parameters as a 'csv' file, the actual Buddhabrot matrix (BDB) together with your parameters as a RData object, and an png file with the image you generate.

Warning for windows users: At this moment the shared object for windows (dll) does not work with doParallel, so multithreading is not possible.

Warning for Raspberry Pi users: At this moment the shared object for Raspberry does not work with doParallel since it is unable to allocate enough memory (Raspberry Pi 2 B.

TODO

* Include inv function in the shared object (plots orbits inside Mandelbrot set).
* Include zooming functionality in the shared object.
* Include shared object switch to Select c values uniformly (consecutive values from the complex plane) or randomly (current behaviour).
* Implement Metropolis-Hastings algorithm to enable practical zooming.
* Explore alternative ways for parallel processing other than ForEach (i.e. snow).
* Explore parallel processing using clusters.
* Explore ways to equalize image histogram.
* Export R object to python.



