\name{freqtab}
\alias{freqtab}
\title{Frequency table}

\description{
Make relative or absolute frequency tables.
}

\usage{
freqtab(veg, clust, relfr = TRUE, sort, limit = 0, dec = 0, ...) 
}

\arguments{
\item{veg}{Vegetation dataframe}
\item{clust}{Vector with cluster information with length equal to number of rows of veg}
\item{relfr}{Relative or absolute frequency}
\item{sort}{Vector of cluster names to sort columns}
\item{limit}{Frequency limit}
\item{dec}{Number of decimals}
\item{...}{additional arguments}
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }

\keyword{misc}