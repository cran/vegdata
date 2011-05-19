\name{syntab}
\alias{syntab}
\alias{print.syntab}
\alias{freqtab}
\title{Frequency table}

\description{
Make relative or absolute frequency tables.
}

\usage{
syntab(veg, clust, type = c('rel','abs','mean.cover'), fullnames=FALSE, limit=0, mupa=NULL, alpha=0.05, minstat=0, dec=0, ...)
}

\arguments{
\item{veg}{Vegetation dataframe}
\item{clust}{Vector with cluster information with length equal to number of rows of veg}
\item{type}{Relative or absolute frequency, mean species response values or strength of association (see function multipatt in package indicspecis).}
\item{fullnames}{Replace rownames (LETTERCODES) with full scientific names.}
\item{limit}{Minimum value to display.}
\item{dec}{Number of decimals in result.}
\item{mupa}{Either logical for (not) using multipatt from package indispecies to detect significance of cluster association strength or supply output from previous use of multipatt.}
\item{alpha}{Significance threshold.}
\item{minstat}{Minimal indicator value}
\item{...}{additional arguments}
}

\author{Florian Jansen \email{jansen@uni-greifswald.de} }

\examples{
data(elbaue)

clust <- vector('integer', nrow(elbaue.env))
clust[elbaue.env$MGL < -50 & elbaue.env$SDGL < 50] <- 1
clust[elbaue.env$MGL < -50 & elbaue.env$SDGL >= 50] <- 2
clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL >= 50] <- 3
clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL < 50] <- 4
# syntab(elbaue, clust, limit=30)

levels(clust) <- c('dry.ld','dry.hd', 'wet.hd','wet.ld')

\dontrun{
syntab(elbaue, clust, limit=30, mupa=TRUE)
}
}

\keyword{misc}

