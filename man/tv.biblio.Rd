\encoding{UTF-8}
\name{tv.biblio}
\alias{tv.biblio}
\title{Check bibliographic references from Turboveg codes}

\description{Check bibliographic references from Turboveg codes}

\usage{
tv.biblio(x='all', db, dict = tv.dict(db), quiet=FALSE, tv_home, iconv = "WINDOWS-1250",
...)
}

\arguments{
\item{x}{Turboveg reference code(s), e.g. "000001"}
\item{db}{Database name. Needed to select approriate TV Dictionary folder.}
\item{quiet}{If you want to print the reference to the screen.}
\item{tv_home}{Turbowin installation path. If not specified function \code{\link{tv.home}} tries to discover.}
\item{dict}{Name of Turboveg Dictionary (term lists for header data) if not the default one.}
\item{iconv}{encoding of Turboveg files if different from your actual environment}
\item{\dots}{additional arguments}
}

\value{Dataframe of (selected) biblioreferences (when assigned to an object).}

\seealso{\code{\link{tv.site}}}

\author{Florian Jansen \email{jansen@uni-greifswald.de} }

\keyword{misc, survey}
