#### Libraries ####

library(Rcpp)           # Interface with C++ Shared Object
library(doParallel)     # Multithreading support
library(foreach)
library(moments)        # Provides skewness fx

library(tcltk2)         # tcltk GUI
library(tcltk)          # tcltk GUI
library(tkrplot)        # tcltk GUI plot
library(rPython)        # Python interface/ uses png library to export 16bit png
library(cape)           # To rotate matrix


options(digits=16)


#### OS Settings ####

OS = "LIN"             #Win = 32 bit windows (i.e. WinXP)
                        #Winx64 = 64 bit windows (i.e. Win7)
                        #Lin (Linux)
                        #MAC (MacOS)
                        #RPI (Raspberry Pi 32 bit)

if (OS == "Winx64") {
  OSSO = paste(OS, ".dll", sep="")
} else if (OS == "Win") {
  OSSO = paste(OS, ".dll", sep="")
} else if (OS == "LIN") {
  OSSO = paste(OS, ".so", sep="")
} else if (OS == "MAC") {
  OSSO = paste(OS, ".so", sep="")
} else if (OS == "RPI") {
  OSSO = paste(OS, ".so", sep="")
}

#Save.Dir = "/home/marino/Dropbox/Fractals/f(R)actal/Buddhab(R)ot/buddhabrot2/objects/"
#setwd("C:/Users/fernamar/Tmp/Dropbox/Fractals/f(R)actal/Buddhab(R)ot/buddhabrot2")

#### Buddabrot function/call ####

buddhabrot <- function(x,        # x limits
                       y,        # y limits
                       nx,       # x resolution
                       ny,       # y resolution
                       bailout,  # bailout value (value above which a point scapes) 
                                 # Default 4. Increasing it improves detail
                       dwell_limit,     # maximun number of dwell_limitations. More dwell_limitations, more detail
                       keep,     # Percentage of dwell_limitations keept. Takes the last %
                                 # of the range (i.e. 5 will take the last 5% of the 
                                 # dwell_limitations). Default 100 (include all) Lower values
                                 # enhance detail
                       batchsize, # number of random points. Larger numbers will produce
                                  # less grainy images
                       #inv      # Not implemented. plots mbrot or
                                 # non-mbrot orbits (anti buddhabrot)
                       power,    # Default 2. Use higher to produce alternative buddhabrots
                       seed     # Random seed needed fot Multithreaded calculations

)           
  
{
  
  # This is the call to the C++ function itself
  the.set = .Call("buddhabrot",
                  nx = as.integer(nx),
                  ny = as.integer(ny),
                  bailout = as.integer(bailout),
                  dwell_limit = as.integer(dwell_limit),
                  keep = as.integer(keep),
                  batchsize = as.integer(batchsize),
                  power = as.integer(power),
                  seed = as.integer(seed)
  )
  # Create a list with elements x, y and z,
  # suitable for image(), persp(), etc. and return it.
  return(z = matrix(the.set, ncol = ny, byrow = T))
}

#dyn.load(paste("./libs/buddhabrot.", OSSO, sep="")) 


##### Juliabrot function/call ####

juliabrot <- function(x,        # x limits
                      y,        # y limits
                      nx,       # x resolution
                      ny,       # y resolution
                      bailout,  # bailout value (value above which a point scapes) 
                      # Default 4. Increasing it improves detail
                      iter,     # maximun number of iterations. More iterations, more detail
                      keep,     # Percentage of iterations keept. Takes the last %
                      # of the range (i.e. 5 will take the last 5% of the 
                      # iterations). Default 100 (include all) Lower values
                      # enhance detail
                      batchsize, # number of random points. Larger numbers will produce
                      # less grainy images
                      #inv      # Not implemented. plots mbrot or
                      # non-mbrot orbits (anti buddhabrot)
                      power,    # Default 2. Use higher to produce alternative buddhabrots
                      seed,      # Random seed needed fot Multithreaded calculations
                      re,
                      im
)           

{
  
  # This is the call to the C++ function itself
  the.set = .Call("juliabrot",
                  nx = as.integer(nx),
                  ny = as.integer(ny),
                  bailout = as.integer(bailout),
                  iter = as.integer(iter),
                  keep = as.integer(keep),
                  batchsize = as.integer(batchsize),
                  power = as.integer(power),
                  seed = as.integer(seed),
                  re = as.numeric(re),
                  im = as.integer(im)
  )
  # Create a list with elements x, y and z,
  # suitable for image(), persp(), etc. and return it.
  return(z = matrix(the.set, ncol = ny, byrow = T))
}


#### Buddabrot settings ####

re <- 0
im <- 0
x = c(-2, 2)
y = c(-2, 2)

#for ( i in 1:4) {

nx = 1000
ny = nx
power = 2
bailout = 4
inv = 1
dwell_limit = 1000
#if (i==2) { keep <-50}
#else if (i == 3) { keep <- 10}
#else if (i == 4) { keep <- 1}
#else {keep <- 90}
keep = 100
#if (i==2) { threads <-2}
#else if (i == 3) { threads <- 3}
#else if (i == 4) { threads <- 4}
#else if (i == 5) { threads <- 5}
#else if (i == 6) { threads <- 6}
#else if (i == 7) { threads <- 7}
#else if (i == 8) { threads <- 8}
#else {threads <- 1}
threads = 4 #use foreach and doMC (multicore)
hits_pixel = 50
batchsize = nx*nx*hits_pixel


#### Load GUI ####

load("./GUIc.RData")

GUIc()

