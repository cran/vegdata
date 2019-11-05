\name{tv.obs}
\alias{tv.obs}
\title{Dataframe of plot-species observations directly from Turboveg}

\description{Dataframe of plot-species observations directly from Turboveg.}

\usage{
tv.obs(db, tv_home, ...)
}

\arguments{
\item{db}{Name of your Turboveg database. This is the directory name containing tvabund.dbf, tvhabita.dbf and tvwin.set. Please include pathnames below but not above Turbowin/Data.}

\item{tv_home}{Turbowin installation path. If not specified function \code{\link{tv.home}} tries to discover.}
\item{\dots}{additional arguments}
}

\value{Data.frame of species occurrences in Turboveg format, that is every occurrence is a row with relev\'{e} number, species number, layer, cover code and optional additional species-plot information.}

\seealso{\code{\link{tv.veg}}}

\examples{
\dontrun{
# Turboveg installation needed
obs <- tv.obs('taxatest')
head(obs)
}
}

\author{Florian Jansen \email{florian.jansen@uni-rostock.de}}

\keyword{misc}
