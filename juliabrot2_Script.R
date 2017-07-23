#######################################################################
# Function to calculate the Mandelbrot set. This function uses .Call  #
# and Rcpp in order to perform the calculations faster.               #
#                                                                     #
# Inspired by Mario dos Reis. September 2003                          #
# Rewritten by Marino Fernandez. August, 2014  		              #
#######################################################################

#######################################################################
# Required Libraries                           	                      #
#######################################################################

library(Rcpp)           # Interface with Shared library
library(doParallel)     # Multithreading support
library(moments)
library(rPython)


#######################################################################
# Settings                                    	                      #
#######################################################################

OS = "Winx64"             #Win = 32 bit windows (i.e. WinXP)
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

#######################################################################
# buddhabrot function, loads shared object       	              #
#######################################################################

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

#dyn.load(paste("./libs/buddhabrot2.", OSSO, sep="")) 
#dyn.load(paste("./src/buddhabrot.so", sep="")) 
dyn.load(paste("./src/Juliabrot.so", sep="")) 

is.loaded("juliabrot")


#######################################################################
# Buddhabrot Parameters                                               #
#######################################################################

x = c(-2, 2)
y = c(-2, 2)

#for ( i in 1:4) {

nx = 1000
ny = nx
power = 2
bailout = 4
inv = 1
iter = 1000
keep = 100
threads = 4 #use foreach and doMC (multicore)
batchsize = nx*nx*50
re <- 0.285
im <- 0.01


#######################################################################
# Multithreaded call                                                  #
#######################################################################

registerDoParallel(cores=threads)
#cl <- makeCluster(detectCores())
#hostlist <- ("marinofernandez@192.168.1.112")
#hostlist <- ("barbarajimenez@192.168.1.119")
#cl <- makeCluster(hostlist, rscript="/usr/bin/Rscript")
#registerDoParallel(cl)
set.seed(1)

if(threads == 0){
  ptimea <- system.time({
    BDBa <-  
      juliabrot(x,y,nx,ny, bailout, iter,  keep,  batchsize/2, power, sample(1:100000000,1,replace=F), re, im)
    
  })
  
} else {
  ptimea <- system.time({
    BDBa <- foreach(i=1:threads, .combine='+') %dopar% {
      juliabrot(x,y,nx,ny, bailout, iter,  keep,  round(batchsize/threads, 0)/2, power, sample(1:100000000,1,replace=F), re , im)
    }
  })
  
}




#######################################################################
# Save Multithreaded image (png)                                      #
#######################################################################

BDBr <- BDBa[,nrow(BDBa):1]
BDBd <- BDBa+BDBr
sumBDB <- sum(BDBd)
hitpixel <- sum(BDBd)/(nx*nx)
maxBDB <- max(BDBd)
minBDB <- min(BDBd)
meanBDB <- mean(BDBd)
sdBDB <- sd(BDBd)
ptimea <- round(ptimea[3],1)
skew <- skewness(as.vector(BDBd))

dev.new()
num.col <- length(unique(c(BDBd)))
grey=c(grey.colors(num.col, start = 0, end = 1))
par(mar=c(2, 2, 4, 1), mgp=c(1,0,0))
#png(filename=paste(filetxt,".png", sep=""), nx, ny, type = "cairo", bg = "white")

image(seq(x[1], x[2], len = nx), seq(y[1], y[2], len = ny),  
      cex.main = 0.8, cex.lab = 0.8, xlab="Real number", ylab="Imaginary number",
      main=c(paste("Buddhabrot MT (", threads,"),  size ", nx, ", Power ", power, 
                   "\nbailout ", bailout, ", iter ", iter, " (kept last ", keep, "%)", 
                   "\nSampling x", batchsize/(nx*nx), 
                   "\nCompute time ", round(ptimea,2), "s", sep="")),
      BDBd, col=grey, ylim = y, xlim = x, ax=F, useRaster=TRUE)
axis(1, cex.axis = 0.7, axTicks(1), format(axTicks(1), scientific = F))
axis(2, cex.axis = 0.7, axTicks(2), format(axTicks(2), scientific = F))

#dev.off()

#}

 filename16b <- paste(filetxt, "-16b.png", sep="")
  
  BDBimg <- (BDBd/max(BDBd))*((2^16)-1)
  
  python.exec("import png")
  
  python.assign("BDBimg", BDBimg)
  
  python.assign("size", nx)
  python.exec("imgWriter = png.Writer(size, size, greyscale=True, alpha=False, bitdepth=16)")
  python.assign("filename16b", filename16b)
  python.exec("f = open(filename16b, \"wb\")")
  python.exec("imgWriter.write(f, BDBimg)")


#######################################################################
# Save Multithreaded Object (BDBa) (RData)                            #
#######################################################################



filetxt <- paste("./objects/", bailout,"p",power,"-",(nx*ny)/1000000,"Mpixel-",iter/1000,"Ki(",keep,")-x",batchsize/(nx*ny),
                 "-MT(",threads,")-computetime",-ptimea, sep="")

save(BDBd,nx,power,bailout,inv,iter,keep,threads,batchsize, sumBDB, hitpixel, maxBDB, skew, ptimea,
     file=paste(filetxt,".RData", sep=""))

cat(paste("power,bailout,inv,nx,size,oversampling,batchsize,iter,kept,threads,ptime,hits,hits-pixel,",
          "max-value,min-value,Mean,SD,Skew",sep=""), file=paste(filetxt,".csv", sep=""),sep="\n")
cat(paste(power,",",format(bailout, scientific=FALSE),",", inv,",",nx ,",", nx*nx/1000000,",",batchsize/(nx*ny),",",
          batchsize,",",iter,",",keep,",",threads,",", round(ptimea,1),",", sumBDB ,",",round(hitpixel,2),",",
          maxBDB,",", minBDB,",",round(meanBDB,2),",", round(sdBDB,2),",",round(skew,3) ,sep=""), 
    file=paste(filetxt,".csv", sep=""),sep="\n",append=TRUE)

