\name{syntab}
\alias{syntab}
\alias{print.syntab}
\alias{freqtab}
\title{Frequency tables}

\description{
Make relative or absolute frequency tables.
}

\seealso{
package indicspecies from M. Cac\'{e}res with function multipatt for indicator species analysis along multiple cluster combinations
}

\usage{
syntab(veg, clust, type = c('rel','abs','mean.cover'), fullnames=FALSE, limit=0, mupa=NULL, alpha=0.05, minstat=0, dec=0, refl, ...)
}

\arguments{
\item{veg}{Vegetation dataframe}
\item{clust}{Vector with cluster information with length equal to number of rows of veg}
\item{type}{Relative or absolute frequency, mean species response values or strength of association (see function multipatt in package indicspecis).}
\item{fullnames}{Replace rownames (LETTERCODES) with full scientific names.}
\item{limit}{Minimum value to display.}
\item{mupa}{Either logical for (not) using multipatt from package indispecies to detect significance of cluster association strength or supply output from previous use of multipatt.}
\item{alpha}{Significance threshold.}
\item{minstat}{Minimal indicator value}
\item{dec}{Number of decimals in result.}
\item{refl}{Name of Turboveg taxonomic reference list to use for fullnames.}
\item{...}{additional arguments}
}

\author{Florian Jansen \email{jansen@uni-greifswald.de} }

\examples{
\dontrun{
elbaue <- tv.veg('elbaue')
elbaue.env <- tv.site('elbaue')
clust <- vector('integer', nrow(elbaue.env))
clust[elbaue.env$MGL < -50 & elbaue.env$SDGL < 50] <- 1
clust[elbaue.env$MGL < -50 & elbaue.env$SDGL >= 50] <- 2
clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL >= 50] <- 3
clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL < 50] <- 4
levels(clust) <- c('dry.ld','dry.hd', 'wet.hd','wet.ld')
syntab(elbaue, clust, limit=30, mupa=TRUE, fullnames=TRUE)
}
}

\keyword{misc}
