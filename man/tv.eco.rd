\name{tv.eco}
\alias{tv.eco}
\title{Load ecological species traits from Turboveg reference list}

\usage{
tv.eco(db, eco = 'ecodbase', refl, tv_home) 
}

\arguments{
\item{db}{Path name to the Turboveg database directory}
\item{eco}{Name of species trait DBase file, default is 'ecodbase'}
\item{refl}{Name of taxonomic reference list}
\item{tv_home}{Turbowin installation path}
}

\description{
Loading Turboveg ecodbase and do basic data evaluation. Empty columns are eliminated and warnings about possibly wrong '0' values are performed
}

\details{
You can use the final output line to make a summary statistic for attributes with potentially misleading '0' values. Just delete the \" at beginning and end.
  }

\value{
data.frame of ecological traits, see \code{metainfo(refl, eco=TRUE)}.
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }
\keyword{misc}