#### Libraries ####

library(Rcpp)               # Interface with Shared library
library(doParallel)         # Multithreading support
library(moments)	          # skewness function
library(rPython)	          # 16 bit png support


#### OS Settings ####

OS = "LIN"            #Win = 32 bit windows (i.e. WinXP)
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


#### Buddhabrot Function ####

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

dyn.load(paste("./libs/buddhabrot2.", OSSO, sep="")) 
#dyn.load(paste("./src/buddhabrot2.so", sep="")) 
is.loaded("buddhabrot")


#### Buddhabrot Parameters ####

re <- 0
im <- 0
x = c(-2, 2)
y = c(-2, 2)

#for ( i in 1:4) {

nx = 10000
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
hits_pixel = 100
batchsize = nx*nx*hits_pixel


#### Buddhabrot Call ####

registerDoParallel(cores=threads)
#cl <- makeCluster(detectCores())
#hostlist <- ("marinofernandez@192.168.1.112")
#hostlist <- ("barbarajimenez@192.168.1.119")
#cl <- makeCluster(hostlist, rscript="/usr/bin/Rscript")
#registerDoParallel(cl)
set.seed(1)

if(threads == 0){
  ptime <- system.time({
    BDB <-  
      buddhabrot(x,y,nx,ny, bailout, dwell_limit,  keep,  batchsize/2, power, sample(1:100000000,1,replace=F))
    
  })
  
} else {
  ptime <- system.time({
    BDB <- foreach(i=1:threads, .combine='+') %dopar% {
      buddhabrot(x,y,nx,ny, bailout, dwell_limit,  keep,  round(batchsize/threads, 0)/2, power, sample(1:100000000,1,replace=F))
    }
  })
  
}


#### Save image / Uses mirror image to dupicate hit count / Only works if im == 0 ####

BDBr <- BDB[,nrow(BDB):1]
BDBd <- BDB+BDBr
BDBs <- abs(BDB-BDBr)
sumBDB <- sum(BDBd)
hitpixel <- sum(BDBd)/(nx*nx)
maxBDB <- max(BDBd)
minBDB <- min(BDBd)
meanBDB <- mean(BDBd)
sdBDB <- sd(BDBd)
ptime <- round(ptime[3],1)

dif <- round(sum(BDBs)/sum(BDB)*100,1)
hazzy <- round(mean(BDBd)/max(BDBd)*100,1)
  

filetxt <- paste("./objects/", bailout,"p",power,"-",(nx*ny)/1000000,"Mpixel-",dwell_limit/1000,"Ki(",keep,")-x",batchsize/(nx*ny),
                 "-D",dif,"-H",hazzy, "-MT(",threads,")-computetime",-ptime, sep="")

#dev.new()
num.col <- length(unique(c(BDBd)))
grey=c(grey.colors(num.col, start = 0, end = 1))
par(mar=c(2, 2, 4, 1), mgp=c(1,0,0))
png(filename=paste(filetxt,".png", sep=""), nx, ny, type = "cairo", bg = "white")

image(seq(x[1], x[2], len = nx), seq(y[1], y[2], len = ny),  
      cex.main = 0.8, cex.lab = 0.8, xlab="Real number", ylab="Imaginary number",
      main=c(paste("Buddhabrot MT (", threads,"),  size ", nx, ", Power ", power, 
                   "\nbailout ", bailout, ", dwell_limit ", dwell_limit, " (kept last ", keep, "%)", 
                   "\nSampling x", batchsize/(nx*nx), 
                   "\nCompute time ", round(ptime,2), "s", sep="")),
      BDBd, col=grey, ylim = y, xlim = x, ax=F, useRaster=TRUE)
axis(1, cex.axis = 0.7, axTicks(1), format(axTicks(1), scientific = F))
axis(2, cex.axis = 0.7, axTicks(2), format(axTicks(2), scientific = F))

dev.off()

#}


#### Save image (16 bit png), using python ####

  filename16b <- paste(filetxt, "-16b.png", sep="")
  BDBimg <- (BDBd/max(BDBd))*((2^16)-1)
  python.exec("import png")
  python.assign("BDBimg", BDBimg)
  python.assign("size", nx)
  python.exec("imgWriter = png.Writer(size, size, greyscale=True, alpha=False, bitdepth=16)") 
  python.assign("filename16b", filename16b)
  python.exec("f = open(filename16b, \"wb\")")
  python.exec("imgWriter.write(f, BDBimg)")


#### Save BDB Object  (RData)  ####

save(BDBd,nx,power,bailout,inv,dwell_limit,keep,threads,batchsize, sumBDB, hitpixel, maxBDB,  ptime,
     file=paste(filetxt,".RData", sep=""))

cat(paste("power,bailout,inv,nx,size,oversampling,batchsize,dwell_limit,kept,threads,ptime,hits,hits-pixel,",
          "max-value,min-value,Mean,SD,Skew",sep=""), file=paste(filetxt,".csv", sep=""),sep="\n")
cat(paste(power,",",format(bailout, scientific=FALSE),",", inv,",",nx ,",", nx*nx/1000000,",",batchsize/(nx*ny),",",
          batchsize,",",dwell_limit,",",keep,",",threads,",", round(ptime,1),",", sumBDB ,",",round(hitpixel,2),",",
          maxBDB,",", minBDB,",",round(meanBDB,2),",", round(sdBDB,2) ,sep=""), 
    file=paste(filetxt,".csv", sep=""),sep="\n",append=TRUE)

