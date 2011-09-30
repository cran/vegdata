\name{tv.site}
\alias{tv.site}
\title{Load site data from Turboveg Database}

\usage{
tv.site(db, tv_home, quiet=FALSE, iconv, common.only = TRUE,...)
}

\arguments{
  \item{db}{Name of your Turboveg database. Directory name containing tvabund.dbf, tvhabita.dbf and tvwin.set.}
  \item{tv_home}{Turbowin installation path. Optional, if Turbowin is either on "C:/turbowin" or "C:/Programme/Turbowin".}
  \item{quiet}{Suppress messages}
  \item{iconv}{If you get an error about unrecognized symbols, you may want to convert characters from one encoding to another. Please provide a character vector of length two with the names of source and target encoding, see \code{\link{iconv}}}
  \item{common.only}{Import only header data which occur in all databases with the same name.}
  \item{...}{Additional options like \code{dec} for type.convert}
}


\description{
Loading Turboveg header data and do basic data evaluation. Empty columns are eliminated and warnings about possibly wrong '0' values are performed
}

\details{
Please specify pathnames below but not above Turbowin/Data. Can be a single database or a character vector of multiple databases. In the latter case

You can use the final output line to make a summary statistic for attributes with potentially misleading '0' values. Just delete the \" at beginning and end.
  }

\value{
data.frame of site variables.
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }

\keyword{misc, survey}