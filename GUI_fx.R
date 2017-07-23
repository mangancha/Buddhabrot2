#### Libraries ####

library(compiler)


#### GUI  ####

GUI <- function(env = parent.frame()) {


  #### GUI functions ####

plotTk <- function() {
 
    BDBr <- BDB[,nrow(BDB):1]
    BDBd <- BDB+BDBr
    BDBs <- abs(BDB-BDBr)
    dif <- round(sum(BDBs)/sum(BDB)*100,1)
    hazzy <- round(mean(BDBd)/max(BDBd)*100,1)
    
    assign("dif", dif, envir = .GlobalEnv)
    assign("hazzy", hazzy, envir = .GlobalEnv)
  
    x = c(-2, 2)
    y = c(-2, 2)
    
    num.col <- length(unique(c(BDBd)))
    grey=c(grey.colors(num.col, start = 0, end = 1))
    par(mar=c(2, 2, 2, 2), mgp=c(1,0,0))
    image(seq(x[1], x[2], len = nx), seq(y[1], y[2], len = nx), 
          BDBd, col=grey, ylab = "Imaginary", 
          xlab = "Real", useRaster=TRUE)

  
}

plotTk_JLB <- function() {
  
  x = c(-2, 2)
  y = c(-2, 2)
  
  num.col <- length(unique(c(log(JLB))))
  grey=c(grey.colors(num.col, start = 0, end = 1))
  par(mar=c(2, 2, 2, 2), mgp=c(1,0,0))
  image(seq(x[1], x[2], len = nx), seq(y[1], y[2], len = nx), 
        log(JLB), col=grey, ylab = "Imaginary", 
        xlab = "Real", useRaster=TRUE)
  
  
}

clickPlot <- function(x, y) {
  width <<- as.numeric(tclvalue(tkwinfo("reqwidth", img))) 
  height <<- as.numeric(tclvalue(tkwinfo("reqheight", img))) 
  
  xp <- c(-2, 2)
  yp <- c(-2, 2)
  xsc <- abs(xp[1]-xp[2])
  ysc <- abs(yp[1]-yp[2])
  
  reabs <-  ((xsc*1.25) / 2)
  imabs <- ((ysc*1.25) / 2)
  
  
  x <<- (((as.numeric(x)/width) * 4 ) - 2 ) * 1.125
  y <<- (((1 - (as.numeric(y)/height)) * 4) - 2 ) * 1.125
  #x <<- (as.numeric(x))
 # y <<- (1 - (as.numeric(y))
  click <<- TRUE
  

  
  GOclick()
  
  tkrreplot(img2)
  click <<- FALSE
}

GOclick <- function() {
  
  dyn.load("./src/Juliabrot.so") 
  
  
  threads<- as.numeric(tclvalue(Threads))
  re<- x
  im<- y
  nx<- as.numeric(tclvalue(Nx))
  ny <- nx
  dwell_limit<- as.numeric(tclvalue(Dwell_limit))
  hits_pixel<- as.numeric(tclvalue(Hits_pixel))
  batchsize <- nx*nx*hits_pixel
  

  
  
  msg <- paste("x: ", x, " y: ", y, 
               "\nwidth: ", width, " height: ", height, 
               sep = "")
  
  tkmessageBox(message = msg)
  
  
  
  set.seed(14)
  if(threads == 0){
    ptime <- system.time({
      JLB <-  
        juliabrot(x,y,nx,ny, bailout, dwell_limit,  keep,  batchsize/2, power, sample(1:100000000,1,replace=F), re, im)
      
    })
    
  } else {
    ptime <- system.time({
      JLB <- foreach(i=1:threads, .combine='+', .inorder = FALSE) %dopar% {
        juliabrot(x,y,nx,ny, bailout, dwell_limit,  keep,  round(batchsize/threads, 0)/2, power, sample(1:100000000,1,replace=F), re, im)
      }
    })
    
  }
  
  
  
  
  ptime <- round(ptime[3],1)
  assign("JLB", JLB, envir = .GlobalEnv)
  assign("threads", threads, envir = .GlobalEnv)
  assign("ptime", ptime, envir = .GlobalEnv)
  assign("re", re, envir = .GlobalEnv)
  assign("im", im, envir = .GlobalEnv)
  assign("nx", nx, envir = .GlobalEnv)
  assign("ny", ny, envir = .GlobalEnv)
  assign("batchsize", batchsize, envir = .GlobalEnv)
  assign("dwell_limit",dwell_limit, envir = .GlobalEnv)
  if (im == 0) {
    assign("hits_pixel", hits_pixel*2, envir = .GlobalEnv)
  }
 
  dyn.unload("./src/Juliabrot.so") 

  
}

GO <- function() {
  

  dyn.load("./src/buddhabrot2.so") 
  
  
  threads<- as.numeric(tclvalue(Threads))
  re<- as.numeric(tclvalue(Re))
  im<- as.numeric(tclvalue(Im))
  nx<- as.numeric(tclvalue(Nx))
  ny <- nx
  dwell_limit<- as.numeric(tclvalue(Dwell_limit))
  hits_pixel<- as.numeric(tclvalue(Hits_pixel))
  batchsize <- nx*nx*hits_pixel
  
  
  
  
  msg <- paste("re: ", re, " im: ", im, " threads: ", threads,
               "\nnx: ", nx, " dwell limit: ", dwell_limit, " hits_pixel: ", hits_pixel,
               
               
               sep = "")
  
  tkmessageBox(message = msg)
  
  
  if (im == 0) {
    hits_pixel = hits_pixel/2
  }
  
  set.seed(14)
  if(threads == 0){
    ptime <- system.time({
      BDB <-  
        buddhabrot(x,y,nx,ny, bailout, dwell_limit,  keep,  batchsize/2, power, sample(1:100000000,1,replace=F))
      
    })
    
  } else {
    registerDoParallel(cores=threads)
    ptime <- system.time({
      BDB <- foreach(i=1:threads, .combine='+', .inorder = FALSE) %dopar% {
        buddhabrot(x,y,nx,ny, bailout, dwell_limit,  keep,  round(batchsize/threads, 0)/2, power, sample(1:100000000,1,replace=F))
      }
    })
    
  }
  
  
  console_output <- paste("Done!", sep = "")
  
  
  
  tkdestroy(window$env$txtWidget)
  
  window$env$txtWidget <- tktext(console, height=5)
  tkinsert(window$env$txtWidget,"end",paste(console_output,collapse="\n"))
  
  tkpack(window$env$txtWidget, ipadx = 0, ipady = 0, padx = 0, pady = 0,  fill = "x")
  
  
  
  ptime <- round(ptime[3],1)
  assign("BDB", BDB, envir = .GlobalEnv)
  assign("threads", threads, envir = .GlobalEnv)
  assign("ptime", ptime, envir = .GlobalEnv)
  assign("re", re, envir = .GlobalEnv)
  assign("im", im, envir = .GlobalEnv)
  assign("nx", nx, envir = .GlobalEnv)
  assign("ny", ny, envir = .GlobalEnv)
  assign("batchsize", batchsize, envir = .GlobalEnv)
  assign("dwell_limit",dwell_limit, envir = .GlobalEnv)
  if (im == 0) {
    assign("hits_pixel", hits_pixel*2, envir = .GlobalEnv)
  }
  
  
  replot(FALSE)
  dyn.unload("./src/buddhabrot2.so") 
}

replot <- function(loadBDB) {
  
  #tkdestroy(window$env$plot)
  #window$env$plot <- tkrplot(centerpanel1, plotTk, 
  #	             hscale = 1.5, vscale = 1.5)
  #tkpack(window$env$plot)
  tkrreplot(img)
  
  
  
  tkdestroy(window$env$rei)
  tkdestroy(window$env$reiholder)
  rei <- tclVar(re)
  window$env$reiholder <- tk2label(BDBiInfo, text = "re: ")
  window$env$rei <- tk2label(BDBiInfo, textvariable = rei)
  tkgrid(window$env$reiholder, window$env$rei)
  tkgrid.configure(window$env$reiholder, column = 0, sticky = "w", padx = c(10,0))
  tkgrid.configure(window$env$rei, column = 1, sticky = "w", padx = c(50,0))
  
  tkdestroy(window$env$imi)
  tkdestroy(window$env$imiholder)
  imi <- tclVar(im)
  window$env$imiholder <- tk2label(BDBiInfo, text = "im: ")
  window$env$imi <- tk2label(BDBiInfo, textvariable = imi)
  tkgrid(window$env$imiholder, window$env$imi)
  tkgrid.configure(window$env$imiholder, column = 0, sticky = "w", padx = c(10,0))
  tkgrid.configure(window$env$imi, column = 1, sticky = "w", padx = c(50,0))
  
 
  tkdestroy(window$env$nxi)
  tkdestroy(window$env$nxiholder)
  nxi <- tclVar(nx)
  window$env$nxiholder <- tk2label(BDBiInfo, text = "nx: ")
  window$env$nxi <- tk2label(BDBiInfo, textvariable = nxi)
  tkgrid(window$env$nxiholder, window$env$nxi)
  tkgrid.configure(window$env$nxiholder, column = 0, sticky = "w", padx = c(10,0))
  tkgrid.configure(window$env$nxi, column = 1, sticky = "w", padx = c(50,0))
  
  tkdestroy(window$env$DLi)
  tkdestroy(window$env$DLiholder)
  DLi <- tclVar(dwell_limit)
  window$env$DLiholder <- tk2label(BDBiInfo, text = "dwell limit: ")
  window$env$DLi <- tk2label(BDBiInfo, textvariable = DLi)
  tkgrid(window$env$DLiholder, window$env$DLi)
  tkgrid.configure(window$env$DLiholder, column = 0, sticky = "w", padx = c(10,0))
  tkgrid.configure(window$env$DLi, column = 1, sticky = "w", padx = c(50,0))
  
  tkdestroy(window$env$HPi)
  tkdestroy(window$env$HPiholder)
  HPi  <- tclVar( hits_pixel)
  window$env$HPiholder <- tk2label(BDBiInfo, text = "Hits_pixel: ")
  window$env$HPi <- tk2label(BDBiInfo, textvariable = HPi)
  tkgrid(window$env$HPiholder, window$env$HPi)
  tkgrid.configure(window$env$HPiholder, column = 0, sticky = "w", padx = c(10,0))
  tkgrid.configure(window$env$HPi, column = 1, sticky = "w", padx = c(50,0))
  
 
  #performance <- read.csv("./logs/performance.csv", header = FALSE, sep = ",", quote = "\"",
  #                        dec = ".", fill = TRUE, comment.char = "")
  
  
  tkdestroy(window$env$threads)
  tkdestroy(window$env$threadsholder)
  threads <- tclVar(threads)
  window$env$threadsholder <- tk2label(BDBpInfo, text = "Threads: ")
  window$env$threads <- tk2label(BDBpInfo, textvariable = threads)
  tkgrid(window$env$threadsholder, window$env$threads)
  tkgrid.configure(window$env$threadsholder, column = 0, sticky = "w", padx = c(10,0))
  tkgrid.configure(window$env$threads, column = 1, sticky = "w", padx = c(56,0))
  
  tkdestroy(window$env$ptime)
  tkdestroy(window$env$ptimeholder)
  ptimega <- tclVar(ptime)
  window$env$ptimeholder <- tk2label(BDBpInfo, text = "Render time: ")
  window$env$ptime <- tk2label(BDBpInfo, textvariable = ptimega)
  tkgrid(window$env$ptimeholder, window$env$ptime)
  tkgrid.configure(window$env$ptimeholder, column = 0, sticky = "w", padx = c(10,0))
  tkgrid.configure(window$env$ptime, column = 1, sticky = "w", padx = c(56,0))
  
  
  tkdestroy(window$env$dif)
  tkdestroy(window$env$difholder)
  dif <- tclVar(paste(dif,"%",sep=""))
  window$env$difholder <- tk2label(BDBpInfo, text = "dif: ")
  window$env$dif <- tk2label(BDBpInfo, textvariable = dif)
  tkgrid(window$env$difholder, window$env$dif)
  tkgrid.configure(window$env$difholder, column = 0, sticky = "w", padx = c(10,0))
  tkgrid.configure(window$env$dif, column = 1, sticky = "w", padx = c(56,0))
  
  tkdestroy(window$env$hazzy)
  tkdestroy(window$env$hazzyholder)
  hazzy <- tclVar(paste(hazzy,"%",sep=""))
  window$env$hazzyholder <- tk2label(BDBpInfo, text = "hazzy: ")
  window$env$hazzy <- tk2label(BDBpInfo, textvariable = hazzy)
  tkgrid(window$env$hazzyholder, window$env$hazzy)
  tkgrid.configure(window$env$hazzyholder, column = 0, sticky = "w", padx = c(10,0))
  tkgrid.configure(window$env$hazzy, column = 1, sticky = "w", padx = c(56,0))

  
}



getBDB <- function() {
  
  
  name <- tclvalue(tkgetOpenFile(
    filetypes = "{ {R Files} {.RData} } { {All Files} * }"))
  if (name == "")
    print("No file selected")
  load(name, .GlobalEnv)
  
  replot(TRUE)
  
  
}

saveBDB <- function() {
  
  filename <- paste("./objects/", bailout,"p",power,"-",(nx*ny)/1000000,"Mpixel-",dwell_limit/1000,"Ki(",keep,")-x",batchsize/(nx*ny),
                   "-MT(",threads,")-computetime",-ptime, sep="")
   
  #performance <- read.csv("./logs/performance.csv", header = FALSE, sep = ",", quote = "\"",
  #                        dec = ".", fill = TRUE, comment.char = "")
    
  
  save(	re, im, nx, dwell_limit,  hits_pixel, dif, hazzy,  
        threads, ptime,BDB,
        file=paste(filename,".RData", sep=""))
  
}

saveJPGplot <- function() {
  
  filename <- paste("./objects/", bailout,"p",power,"-",(nx*ny)/1000000,"Mpixel-",dwell_limit/1000,"Ki(",keep,")-x",batchsize/(nx*ny),
                    "-MT(",threads,")-computetime",-ptime, sep="")
  
  #if (loadBDB == FALSE) {
  re<- as.numeric(tclvalue(Re))
  im<- as.numeric(tclvalue(Im))
  nx<- as.numeric(tclvalue(Nx))
  #} 
  
  

  
  if (im == 0) {
    
    BDBr <- BDB[,nrow(BDB):1]
    BDBd <- BDB+BDBr
    
    
    maintitle=c(paste("BDB " , (nx*nx/1000000), "MPixel, ", dwell_limit/1000,"Ki",
                      "\n", hits_pixel, "hits/pixel, ", threads, "T , Compute time ", round(ptime,2), sep=""))
    
    num.col <- length(unique(c(BDBd)))
    grey=c(grey.colors(num.col, start = 0, end = 1))
    
    
    #dev.new()
    
    png(filename=paste(filename,".png", sep=""), nx, nx, type = "cairo", bg = "white")
    image(seq(x[1], x[2], len = nx), seq(y[1], y[2], len = nx),  
          cex.main = 0.8, cex.lab = 0.8, xlab="Real number", ylab="Imaginary number",
          main=c(paste(maintitle, sep="")),
          BDB, col=grey, ax=F, useRaster=TRUE)
    axis(1, cex.axis = 0.7, axTicks(1), format(axTicks(1), scientific = F))
    axis(2, cex.axis = 0.7, axTicks(2), format(axTicks(2), scientific = F))
    
    dev.off()
    
    
    
  } else {
    
    
    
    
    maintitle=c(paste("BDB " , (nx*nx/1000000), "MPixel, ", dwell_limit/1000,"Ki",
                      "\n", hits_pixel, "hits/pixel, ", threads, "T , Compute time ", round(ptime,2), sep=""))
    
    
    num.col <- length(unique(c(BDB)))
    grey=c(grey.colors(num.col, start = 0, end = 1))
    
    
    dev.new()
    
    
    png(filename=paste(filename,".png", sep=""), nx, nx, type = "cairo", bg = "white")
    image(seq(x[1], x[2], len = nx), seq(y[1], y[2], len = nx),  
          cex.main = 0.8, cex.lab = 0.8, xlab="Real number", ylab="Imaginary number",
          main=c(paste("Buddhabrot " , threads,"T ,  size ", nx, ", Power 2", 
                       "\nhits_pixel ", hits_pixel, " Compute time ", round(ptime,2), sep="")),
          BDB, col=grey, ax=F, useRaster=TRUE)
    axis(1, cex.axis = 0.7, axTicks(1), format(axTicks(1), scientific = F))
    axis(2, cex.axis = 0.7, axTicks(2), format(axTicks(2), scientific = F))
    
    dev.off()
    
    
    
  }
  
}

saveJPGimg <- function() {
  
  filename <- paste("./objects/", bailout,"p",power,"-",(nx*ny)/1000000,"Mpixel-",dwell_limit/1000,"Ki(",keep,")-x",batchsize/(nx*ny),
                    "-MT(",threads,")-computetime",-ptime, sep="")
  
  #if (loadBDB == FALSE) {
  re<- as.numeric(tclvalue(Re))
  im<- as.numeric(tclvalue(Im))
  nx<- as.numeric(tclvalue(Nx))
  #} 
  
  

  
  if (im == 0) {
    
    BDBr <- BDB[,nrow(BDB):1]
    BDBd <- BDB+BDBr
    BDBd <- rotate.mat(BDBd)
    BDBd <- rotate.mat(BDBd)
    BDBd <- rotate.mat(BDBd)
    
    
    filename16b <- paste(filename, "-16b.png", sep="")
    
    BDBimg <- (BDBd/max(BDBd))*((2^16)-1)
    
    python.exec("import png")
    
    python.assign("BDBimg", BDBimg)
    
    python.assign("size", nx)
    python.exec("imgWriter = png.Writer(size, size, greyscale=True, alpha=False, bitdepth=16)")
    python.assign("filename16b", filename16b)
    python.exec("f = open(filename16b, \"wb\")")
    python.exec("imgWriter.write(f, BDBimg)")
    
    
  } else {
    
    BDB <- rotate.mat(BDB)
    BDB <- rotate.mat(BDB)
    BDB <- rotate.mat(BDB)
    
    filename16b <- paste(filename, "-16b.png", sep="")
    
    BDBimg <- (BDB/max(BDB))*((2^16)-1)
    
    python.exec("import png")
    
    python.assign("BDBimg", BDBimg)
    
    python.assign("size", nx)
    python.exec("imgWriter = png.Writer(size, size, greyscale=True, alpha=False, bitdepth=16)")
    python.assign("filename16b", filename16b)
    python.exec("f = open(filename16b, \"wb\")")
    python.exec("imgWriter.write(f, BDBimg)")
    
    
  }
  
}

#### Main Window and Tabs ####

window <- tktoplevel()
tktitle(window) <- "Buddhabrot 2"
#tcl("update","idletasks")
#.Tcl("update idletasks")

window$env$nb <- tk2notebook(window, tabs = c("Buddhabrot", "Juliabrot"))
tab1 <- tk2notetab(window$env$nb, "Buddhabrot")
tab2 <- tk2notetab(window$env$nb, "Juliabrot")
tkpack(window$env$nb, fill = "both", expand = TRUE)

rightpanel1 <- tk2frame(tab1, borderwidth = 3,  padding = 0)
centerpanel1 <- tk2frame(tab1, borderwidth = 3, padding = 0)
leftpanel1 <- tk2frame(tab1, borderwidth = 3, relief = "sunken", padding = 0)
tkpack(rightpanel1,  side = "right", expand = FALSE, ipadx = 10, ipady = 10,  fill = "both")
tkpack(centerpanel1,  side = "right", expand = FALSE,  ipadx = 10, ipady = 10,  fill = "both")
tkpack(leftpanel1,  side = "left", expand = FALSE, ipadx = 10, ipady = 10, fill = "both")

rightpanel2 <- tk2frame(tab2, borderwidth = 3,  padding = 0)
centerpanel2 <- tk2frame(tab2, borderwidth = 3, padding = 0)
leftpanel2 <- tk2frame(tab2, borderwidth = 3, relief = "sunken", padding = 0)
tkpack(rightpanel2,  side = "right", expand = FALSE, ipadx = 10, ipady = 10,  fill = "both")
tkpack(centerpanel2,  side = "right", expand = FALSE,  ipadx = 10, ipady = 10,  fill = "both")
tkpack(leftpanel2,  side = "left", expand = FALSE, ipadx = 10, ipady = 10, fill = "both")


#### Menu ####

window$env$menu <- tk2menu(window)
tkconfigure(window, menu = window$env$menu)
window$env$menuFile <- tk2menu(window$env$menu, tearoff = FALSE)
tkadd(window$env$menuFile, "command", label = "Load BDB Object", command = getBDB)
tkadd(window$env$menuFile, "command", label = "Save BDB Object", command = saveBDB)
tkadd(window$env$menuFile, "command", label = "Save JPEG Plot", command = saveJPGplot)
tkadd(window$env$menuFile, "command", label = "Save JPEG img (16bit)", command = saveJPGimg)
tkadd(window$env$menuFile, "command", label = "Quit", command = function() tkdestroy(window))

tkadd(window$env$menu, "cascade", label = "File", menu = window$env$menuFile)


#### Left Panel Tab 1 ####

Threads <- tclVar(threads)
threadframe <-tkframe(leftpanel1)
tkpack(threadframe, padx = c(10,10), pady = 2,  ipadx = 0, fill = "x")
#tkgrid(threadframe, padx = 2, pady = 2)
window$env$threads <- tk2entry(threadframe, width = "7", textvariable = Threads)
tkgrid(tk2label(threadframe, text = "threads:"), window$env$threads,
       padx = 2, pady = 2)	   


coordinates <- tk2labelframe(leftpanel1, text = "BDB Params")
tkpack(coordinates, padx = c(10,10), pady = 2,  ipadx = 0, fill = "x")
#tkgrid(coordinates, padx = 0, pady = 0,ipadx = 2, ipady = 2)

Re <- tclVar(re)
window$env$re <- tk2entry(coordinates, width = "7", textvariable = Re)
window$env$reholder <- tk2label(coordinates, text = "re:")
tkgrid(window$env$reholder, window$env$re)
tkgrid.configure(window$env$reholder, column = 0, sticky = "w", padx = c(10,0))
tkgrid.configure(window$env$re, column = 1, sticky = "w",   padx = c(50,0))


Im <- tclVar(im)
window$env$im <-tk2entry(coordinates, width = "7", textvariable = Im)
window$env$imholder <- tk2label(coordinates, text = "im:")
tkgrid(window$env$imholder, window$env$im)
tkgrid.configure(window$env$imholder, column = 0, sticky = "w", padx = c(10,0))
tkgrid.configure(window$env$im, column = 1, sticky = "w", padx = c(50,0))

Nx <- tclVar(nx)
window$env$nx <-tk2entry(coordinates, width = "7", textvariable = Nx)
window$env$nxholder <- tk2label(coordinates, text = "nx:")
tkgrid(window$env$nxholder , window$env$nx)
tkgrid.configure(window$env$nxholder, column = 0, sticky = "w", padx = c(10,0))
tkgrid.configure(window$env$nx, column = 1, sticky = "w", padx = c(50,0))

Dwell_limit <- tclVar(dwell_limit)
window$env$dwell_limit <-tk2entry(coordinates, width = "7", textvariable = Dwell_limit)
window$env$dwell_limit_holder <- tk2label(coordinates, text = "dwell limit:")
tkgrid(window$env$dwell_limit_holder , window$env$dwell_limit)
tkgrid.configure(window$env$dwell_limit_holder, column = 0, sticky = "w", padx = c(10,0))
tkgrid.configure(window$env$dwell_limit, column = 1, sticky = "w", padx = c(50,0))




window$env$butOK1 <- tk2button(leftpanel1, text = "GO!", width = 0, command = GO) 
tkpack(window$env$butOK1, padx = 0, pady = 0)

#### Left Panel Tab 2 ####

#Not enabled to get Juliabrot click on Mandelbrot
window$env$butOK2 <- tk2button(leftpanel2, text = "GO Julia!", width = 0, command = GO) 
tkpack(window$env$butOK2, padx = 0, pady = 0)


#### Center Panel Tab 1 ####

BDB <- matrix(rep(0,nx*nx), ncol = nx, byrow = T)

img <- tkrplot(centerpanel1, plotTk, hscale = 1.5, vscale = 1.5)
tkbind(img, "<1>", clickPlot)
tkpack(img)

rm(BDB)

center_bottom <- tk2frame(centerpanel1, borderwidth = 3, relief = "sunken", padding = 0)
tkpack(center_bottom,  side = "bottom", expand = FALSE,  ipadx = 0, ipady = 0, padx = c(10,10), pady = 2,  fill = "both")

console <- tk2frame(center_bottom, borderwidth = 3, relief = "sunken", padding = 0)
tkpack(console,  side = "left", expand = FALSE,  ipadx = 0, ipady = 0, padx = c(10,10), pady = 2,  fill = "x")

console_output <- "Hello!"
window$env$txtWidget <- tktext(console, height=5)
tkpack(window$env$txtWidget, ipadx = 0, ipady = 0, padx = 0, pady = 0,  fill = "x")
tkinsert(window$env$txtWidget,"end",paste(console_output,collapse="\n"))

#tkgrid(window$env$console, padx = 0, pady = 0, sticky = "w", padx = c(24,0))



imparfr <- tk2labelframe(center_bottom, text = "Image Parameters", width = 50)
tkpack(imparfr,  side = "right", expand = FALSE, ipadx = 10, ipady = 10,  fill = "x")

Hits_pixel <- tclVar(hits_pixel)
window$env$hits_pixel <-tk2entry(imparfr, width = "7", textvariable = Hits_pixel)
window$env$hits_pixel_holder <- tk2label(imparfr, text = "Hits per pixel:")
tkgrid(window$env$hits_pixel_holder, window$env$hits_pixel)
tkgrid.configure(window$env$hits_pixel_holder, column = 0, sticky = "w", padx = c(10,0))
tkgrid.configure(window$env$hits_pixel, column = 1, sticky = "w", padx = c(10,0))






#console <- tcl("source", file.path(.Library, "tcltk", "exec", "console.tcl"))
#.C("RTcl_ActivateConsole", PACKAGE = "tcltk")
#tkpack(console,  side = "bottom", expand = FALSE,  ipadx = 10, ipady = 10,  fill = "both")


#### Center Panel Tab 2 ####

JLB <- matrix(rep(0,nx*nx), ncol = nx, byrow = T)

img2 <- tkrplot(centerpanel2, plotTk_JLB, hscale = 1.5, vscale = 1.5)
tkpack(img2)

rm(JLB)


#### Right Panel Tab 1 ####

BDBiInfo <- tk2labelframe(rightpanel1, text = "BDB image info")
tkpack(BDBiInfo, padx = c(10,10), pady = 2,  ipadx = 10, fill = "x")

window$env$reiholder <- tk2label(BDBiInfo, text = "re: ")
window$env$rei <- tk2label(BDBiInfo, text = "NA")
tkgrid(window$env$reiholder, window$env$rei)
tkgrid.configure(window$env$reiholder, column = 0, sticky = "w", padx = c(10,0))
tkgrid.configure(window$env$rei, column = 1, sticky = "w", padx = c(50,0))

window$env$imiholder <- tk2label(BDBiInfo, text = "im: ")
window$env$imi <- tk2label(BDBiInfo, text = "NA")
tkgrid(window$env$imiholder, window$env$imi)
tkgrid.configure(window$env$imiholder, column = 0, sticky = "w", padx = c(10,0))
tkgrid.configure(window$env$imi, column = 1, sticky = "w", padx = c(50,0))

window$env$nxiholder <- tk2label(BDBiInfo, text = "nx: ")
window$env$nxi <- tk2label(BDBiInfo, text = "NA")
tkgrid(window$env$nxiholder, window$env$nxi)
tkgrid.configure(window$env$nxiholder, column = 0, sticky = "w", padx = c(10,0))
tkgrid.configure(window$env$nxi, column = 1, sticky = "w", padx = c(50,0))

window$env$DLiholder <- tk2label(BDBiInfo, text = "dwell limit: ")
window$env$DLi <- tk2label(BDBiInfo, text = "NA")
tkgrid(window$env$DLiholder, window$env$DLi)
tkgrid.configure(window$env$DLiholder, column = 0, sticky = "w", padx = c(10,0))
tkgrid.configure(window$env$DLi, column = 1, sticky = "w", padx = c(50,0))

window$env$HPiholder <- tk2label(BDBiInfo, text = "Render: ")
window$env$HPi <- tk2label(BDBiInfo, text = "NA")
tkgrid(window$env$HPiholder, window$env$HPi)
tkgrid.configure(window$env$HPiholder, column = 0, sticky = "w", padx = c(10,0))
tkgrid.configure(window$env$HPi, column = 1, sticky = "w", padx = c(50,0))


BDBpInfo <- tk2labelframe(rightpanel1, text = "Performance")
tkpack(BDBpInfo, padx = c(10,10), pady = 2,   ipadx = 10, fill = "x")



window$env$threadsholder <- tk2label(BDBpInfo, text = "Threads: ")
window$env$threads <- tk2label(BDBpInfo, text = "NA")
tkgrid(window$env$threadsholder, window$env$threads)
tkgrid.configure(window$env$threadsholder, column = 0, sticky = "w", padx = c(10,0))
tkgrid.configure(window$env$threads, column = 1, sticky = "w", padx = c(56,0))

window$env$ptimeholder <- tk2label(BDBpInfo, text = "Render time: ")
window$env$ptime <- tk2label(BDBpInfo, text = "NA")
tkgrid(window$env$ptimeholder, window$env$ptime)
tkgrid.configure(window$env$ptimeholder, column = 0, sticky = "w", padx = c(10,0))
tkgrid.configure(window$env$ptime, column = 1, sticky = "w", padx = c(56,0))

window$env$difholder <- tk2label(BDBpInfo, text = "dif: ")
window$env$dif <- tk2label(BDBpInfo, text ="NA")
tkgrid(window$env$difholder, window$env$dif)
tkgrid.configure(window$env$difholder, column = 0, sticky = "w", padx = c(10,0))
tkgrid.configure(window$env$dif, column = 1, sticky = "w", padx = c(56,0))

window$env$hazzyholder <- tk2label(BDBpInfo, text = "hazzy: ")
window$env$hazzy <- tk2label(BDBpInfo, text = "NA")
tkgrid(window$env$hazzyholder, window$env$hazzy)
tkgrid.configure(window$env$hazzyholder, column = 0, sticky = "w", padx = c(10,0))
tkgrid.configure(window$env$hazzy, column = 1, sticky = "w", padx = c(56,0))


}


#### Compile and save GUI object ####

GUIc <- cmpfun(GUI)
save(GUIc, file = "./GUIc.RData")
