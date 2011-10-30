vegetweb <- function() {
cat('\nFunctions to access data from VegetWeb (http://www.floraweb.de/vegetation/aufnahmen.html),
the National Vegetation database of Germany.\n')
if('RMySQL' %in% installed.packages()[,'Package']) {
cat('\nLibrary RMySQl detected. To access data from VegetWeb, i will download additional code from:
http://geobot.botanik.uni-greifswald.de/download/r_package/vegetweb.R\n')
source('http://geobot.botanik.uni-greifswald.de/download/r_package/vegetweb.r')
cat('Functions vw.site(), vw.veg() now available')
} else 
cat('\nVegetWeb access requires package RMySQL which is not installed until now. Try to install (see vignette("vegdata") and rerun.\n\n')
}

