\name{vegdata-internal}
\alias{tv.home}
\alias{tv.refl}
\alias{first.word}
\alias{reShape.veg}
\alias{TCS.replace}
\alias{TV.replace}
\alias{tv.dict}
\alias{load.taxlist}
\alias{showplot}
\alias{as.data.frame.list}
\alias{decode}

\title{Internal vegdata functions}

\description{
  Internal vegdata functions.
}

\details{
  These are not intended to be called directly by the user.

  tv.home tries to guess the default tv_home directory (\code{'C:\Turbowin'} or \code{'C:\Programme\Turbowin'} or \code{'O:\Turbowin'} on Windows systems and \code{'~/.wine/drive_c/Turbowin'} on Unix systems.
  
As dBase is an old DOS format, Umlaute have been stored in Turboveg using the CP437 code table. This has been changed and Turboveg seems to use a country specific code page now. Change options('tv.iconv') if you run into problems
}

\keyword{internal}
