tv.home <- function(sysPath=FALSE, ...) {
  if(sysPath) tv_home <- file.path(.path.package("vegdata"), "extdata") else
    if(.Platform$OS.type == "unix") tv_home <- paste(Sys.getenv('HOME'),'/.wine/drive_c/Turbowin',sep='') else
    if(.Platform$OS.type == "windows") {
      if(file.access('C:/Turbowin/Popup/tvscale.dbf')==0) tv_home <- 'C:/Turbowin' else {
      if(file.access('O:/Turbowin/Popup/tvscale.dbf')==0) tv_home <- 'O:/Turbowin' else {
      if(file.access('C:/Programme/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'C:/Programme/Turbowin' else
        stop('Default Turbowin installation path not found. Please specify with option tv_home.')
      }}} 
    tv_home
 }
