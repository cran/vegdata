vegetweb <- function() {
cat('Functions to access data from VegetWeb (http://www.floraweb.de/vegetation/aufnahmen.html),
the National Vegetation database of Germany.\n')
if(length(find.package('RMySQL', quiet=TRUE)) > 0)  {
  cat('\nLibrary RMySQl detected. To access data from VegetWeb, I will download additional code from:
  http://geobot.botanik.uni-greifswald.de/download/r_package/vegetweb.R\n')
  source('http://geobot.botanik.uni-greifswald.de/download/r_package/vegetweb.r')
  cat('The functions vw.con(), vw.site(), and vw.veg() are now available.')
  } else 
  cat('\nVegetWeb access requires package RMySQL which seems not to be installed. Try to install (see vignette("vegdata")) and rerun.\n\n')
}

