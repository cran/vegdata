\name{vegdata-internal}
\alias{tv.home}
\alias{tv.refl}
\alias{compRefl}


\title{Internal vegdata functions}

\description{
  Internal vegdata functions.
}

\details{
  These are not to be called by the user.
  tv.home tries to guess the default tv_home directory (\code{'C:\Turbowin'} or \code{'C:\Programme\Turbowin'} or \code{'O:\Turbowin'} on Windows systems and \code{'~/.wine/drive_c/Turbowin'} on Unix systems.
  compRefl(refl1, refl2, tv_home, var = 'ABBREVIAT', keyvar = 'SPECIES_NR', p=TRUE, dif=FALSE, Sink=FALSE) compares two reference lists.
 reload.vegdata installs a new vegdata version from Greifswald Server and load it into R.
}

\keyword{internal}