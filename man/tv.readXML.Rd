\name{tv.readXML}
\alias{tv.readXML}
\title{Read Turboveg XML}

\description{Reads Turboveg XML formatted files species-plot observations and site information to Turboveg database.}

\usage{
tv.readXML(file)
}

\arguments{
  \item{file}{Path name of the Turboveg XML file}
}

\value{
  S3 list with elements tvwin, tvadmin, site, and obs.
}

\seealso{\code{\link{tv.veg}}, \code{\link{tv.site}}}

\author{Florian Jansen \email{florian.jansen@uni-rostock.de}}

\keyword{misc}
