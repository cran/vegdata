\name{tv.traits}
\alias{tv.traits}
\alias{meanTraits}
\alias{tv.eco}
\title{Load species traits from Turboveg reference list}

\usage{
tv.traits(db, trait.db = 'ecodbase.dbf', refl, ...)
}

\arguments{
\item{db}{Path name to the Turboveg database directory}
\item{trait.db}{Name of species trait DBase file, default is 'ecodbase'}
\item{refl}{Name of the taxonomic reference list, if veg is not loaded with tv.veg}
\item{\dots}{additional arguments for tv.traits}
}

\description{
Loading Turboveg ecodbase or any other specified dBase file in this directory and do basic data evaluation. Empty columns are eliminated.
}

\details{
You can use the final output line to make a summary statistic for attributes with potentially misleading '0' values. Just delete the \" at beginning and end.
  }

\value{
data.frame of ecological traits, see \code{metainfo(refl, eco=TRUE)}.
}

\seealso{ \code{\link{isc}}}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }
        
\keyword{misc}