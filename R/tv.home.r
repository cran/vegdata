tv.home <- function(sysPath=FALSE, ...) {
  if(sysPath) tv_home <- file.path(.path.package("vegdata"), "tvdata") else
  if(is.null(getOption('tv_home'))) {
    if(.Platform$OS.type == "unix") if(file.access(paste(Sys.getenv('HOME'),'/.wine/drive_c/Turbowin',sep=''))==0)
      tv_home <- file.path(Sys.getenv('HOME'),'.wine/drive_c/Turbowin') else {
        warning('Default Turbowin installation path not found. \n
##########################################################
Please specify with:
options(tv_home=\"path_to_your_ Turbowin_root\").
##########################################################')
	tv_home <- NULL
	}
    if(.Platform$OS.type == "windows") {
      if(file.access('O:/Turbowin/Popup/tvscale.dbf')==0) tv_home <- 'O:/Turbowin' else {
	if(file.access('C:/Turbowin/Popup/tvscale.dbf')==0) tv_home <- 'C:/Turbowin' else {
	  if(file.access('C:/Programs/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'C:/Programme/Turbowin' else
	    if(file.access('C:/Programme/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'C:/Programme/Turbowin' else
	      if(file.access('D:/Programme/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'D:/Programme/Turbowin' else {
       warning('Default Turbowin installation path not found. \n
##########################################################
Please specify with:
options(tv_home=\"path_to_your_ Turbowin_root\").
##########################################################')
	  tv_home <- NULL
      }}}}
    tv_home
  } else getOption('tv_home')
}

