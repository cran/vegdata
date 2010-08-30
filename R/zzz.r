.First.lib <- function(...) {
  if(!is.null(getOption('tv_home'))) 
     cat('\n Turboveg root directory is set to ',getOption('tv_home'),'.\n') else {
      tv_home <- tv.home()
      if(!is.null(tv_home)) {
      options(tv_home=tv_home)
      cat('\n##########################################################\n',
	  'Turboveg root directory is set to', tv_home,
	  '\nIf you want to change this use: options(tv_home=\"path_to_your_Turbowin_root\")',
	  '\n##########################################################\n')
    }
  }
}

