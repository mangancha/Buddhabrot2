Buddhabrot2
===========
This project includes.

* R scripts (main folder) to render a Buddhabrot (and a Juliabrot) with support for parallel procesing, calling 
on C++ shared objects to speed up rendering.
* Shared objects contained in the lib folder, compiled for 2 platforms (Linux and Arm7 (Raspberry Pi)), 
with instructions to compile them in Windows and MacOS. These shared objects are called by the R scripts.
* The C++ code needed to compile those shared objects using Rcpp (buddhabrot.cpp, xander.cpp, random.cpp) is included in src.

How to use:
* Download to a directory of your choosing. 
* Open buddhabrot.R and set the 'OS' variable to your operating system. 
* You can save a png render, a 16bit png render, a RData object with the BDB object, and a text file
containing the parameters used.

Warning for windows users: At this moment the shared object for windows (dll) does not work with doParallel (fork), so multithreading is not possible. Apparently it is possible to use Parallel
Socket Cluster (PSOCK) in windows, but I have not implemente it.

Warning for Raspberry Pi users: At this moment the shared object for Raspberry does not work with doParallel since it is unable to allocate enough memory (Raspberry Pi 2 B).

TODO

* Include inv function in the shared object (plots orbits inside Mandelbrot set).
* Include shared object switch to Select c values uniformly (consecutive values from the complex plane) or randomly (current behaviour).
* Explore alternative ways for parallel processing other than ForEach (i.e. snow).
* Explore parallel processing using clusters.




