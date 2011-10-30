\encoding{UTF-8}
\name{tv.biblio}
\alias{tv.biblio}
\title{Check bibliographic references from Turboveg codes}

\description{Check bibliographic references from Turboveg codes}

\usage{
tv.biblio(x='all', site, quiet=FALSE, tv_home, ...)
}

\arguments{
\item{x}{Turboveg reference code(s), e.g. "000001"}
\item{site}{If you want to calculate the number of relevés per reference, please indicate the header data, see \code{\link{tv.site}}}
\item{quiet}{If you want to print the reference to the screen.}
\item{tv_home}{Turbowin installation path. If not specified function \code{\link{tv.home}} tries to discover.}
\item{\dots}{additional arguments}
}

\value{Dataframe of (selected) biblioreferences (when assigned to an object).}

\seealso{\code{\link{tv.site}}}

\author{Florian Jansen \email{jansen@uni-greifswald.de} }

\keyword{misc, survey}
