\name{syntab}
\alias{syntab}
\alias{print.syntab}
\alias{freqtab}
\title{Frequency table}

\description{
Make relative or absolute frequency tables.
}

\usage{
syntab(veg, clust, freq = c('rel','abs','mean.cover'), fullnames=FALSE, limit=0, dec=0, mupa=FALSE, alpha=0.05, minstat=0, ...) 
}

\arguments{
\item{veg}{Vegetation dataframe}
\item{clust}{Vector with cluster information with length equal to number of rows of veg}
\item{freq}{Relative or absolute frequency, mean species response values or strength of association (see function multipatt in package indicspecis).}
\item{fullnames}{Replace rownames (LETTERCODES) with full scientific names.}
\item{limit}{Minimum value to display.}
\item{dec}{Number of decimals in result.}
\item{mupa}{Use multipatt function from package indispecies to detect significance of cluster association strength.}
\item{alpha}{Significance threshold.}
\item{minstat}{Minimal indicator value}
\item{...}{additional arguments}
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }
\examples{
veg <- tv.veg('elbaue', sysPath=TRUE)
site <- tv.site('elbaue', sysPath=TRUE)

clust <- vector('integer', nrow(site))
clust[site$MGL < -50 & site$SDGL < 50] <- 1
clust[site$MGL < -50 & site$SDGL >= 50] <- 2
clust[site$MGL >= -50 & site$SDGL >= 50] <- 3
clust[site$MGL >= -50 & site$SDGL < 50] <- 4
levels(clust) <- c('dry.ld','dry.hd', 'wet.hd','wet.ld')

syntab(veg, clust, limit=30, mupa=TRUE)

}

\keyword{misc}