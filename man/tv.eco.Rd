\name{tv.traits}
\alias{tv.traits}
\alias{tv.eco}
\title{Load species traits from Turboveg reference list}

\usage{
tv.traits(db, eco = 'ecodbase.dbf', refl) 
}

\arguments{
\item{db}{Path name to the Turboveg database directory}
\item{eco}{Name of species trait DBase file, default is 'ecodbase'}
\item{refl}{Name of taxonomic reference list}
}

\description{
Loading Turboveg ecodbase or any other specified dBase file in this directory and do basic data evaluation. Empty columns are eliminated and warnings about possibly wrong '0' values are performed
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