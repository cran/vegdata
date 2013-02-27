tv.home <- function() {
  if(is.null(getOption('tv_home'))) {
    if(.Platform$OS.type == "unix") if(file.access(paste(Sys.getenv('HOME'),'/.wine/drive_c/Turbowin', sep=''))==0)
      tv_home <- file.path(Sys.getenv('HOME'),'.wine/drive_c/Turbowin') else {
        cat('\nNo Turbowin installation path found. \n')
	tv_home <- file.path(path.package("vegdata"), "tvdata")
	}
    if(.Platform$OS.type == "windows") {
      if(file.access('O:/Turbowin/Popup/tvscale.dbf')==0) tv_home <- 'O:/Turbowin' else {
	if(file.access('C:/Turbowin/Popup/tvscale.dbf')==0) tv_home <- 'C:/Turbowin' else {
	  if(file.access('C:/Programs/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'C:/Programme/Turbowin' else
	    if(file.access('C:/Programme/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'C:/Programme/Turbowin' else
	      if(file.access('D:/Programme/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'D:/Programme/Turbowin' else {
       cat('\nNo Turbowin installation path found. \n')
	tv_home <- file.path(path.package("vegdata"), "tvdata")
      }}}}
  cat('\n##########################################################',
      '\nTurboveg root directory is set to', tv_home,
      '\nIf you want to change this use: options(tv_home=\"<path_to_your_Turbowin_root>\")',
      '\n##########################################################\n')
  options(tv_home=tv_home)
  } else {
  tv_home <- getOption('tv_home')
#  cat('Turboveg root directory has already been set to "', tv_home,'".\n', sep='')
  }
  invisible(tv_home)
}

