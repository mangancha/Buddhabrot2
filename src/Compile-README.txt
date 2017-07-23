LINUX
export PKG_CXXFLAGS=`Rscript -e "Rcpp:::CxxFlags()"`
export PKG_LIBS=`Rscript -e 'Rcpp:::LdFlags()'`
R CMD SHLIB buddhabrot2.cpp


RASPBERRY
export PKG_CXXFLAGS=`Rscript -e "Rcpp:::CxxFlags()"`
export PKG_LIBS=`Rscript -e 'Rcpp:::LdFlags()'`
R CMD SHLIB buddhabrot2.cpp


WINDOWS
set PATH=%PATH%;D:\tmp\R\Rtools\bin;D:\tmp\R\R-3.2.3\bin\x64\;D:\tmp\R\Rtools\gcc-4.6.3\bin;D:\tmp\R\R-3.2.3\bin
set PKG_CXXFLAGS=-I"d:/tmp/R/R-3.2.3/library/Rcpp/include"
set PKG_LIBS=`Rscript -e 'Rcpp:::LdFlags()'`
R CMD SHLIB buddhabrot2.cpp


MacOS
export PKG_CXXFLAGS=`Rscript -e "Rcpp:::CxxFlags()"`
export PKG_LIBS=`Rscript -e 'Rcpp:::LdFlags()'`
R CMD SHLIB buddhabrot2.cpp
