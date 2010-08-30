\encoding{UTF-8}
\name{vegdata-package}
\alias{vegdata-package}
\alias{vegdata}

\docType{package}
\title{
 Functions to access data from (Turboveg) vegetation databases and evaluate taxon names (with GermanSL)
}
\description{
This package provides a set of functions to load data from (at present: Turboveg) databases. It is also possible to semi-automatically check and adapt scientific plant names.
}

\details{
\tabular{ll}{
Package: \tab vegdata\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2010-10-01\cr
License: \tab GPL version 2 or newer\cr
LazyLoad: \tab yes\cr
}
Use \code{\link{tv.veg}} to prepare data directly for further analyses. Set option \code{\link{tv.taxval}} to \code{TRUE}, if your database is referenced with taxonomic reference list GermanSL and you want taxonomic check and adaptations.\\
For more details see \code{vignette('vegdata')}.
}
\author{
Florian Jansen

Maintainer: Florian Jansen <jansen@uni-greifswald.de>
}

\references{
Jansen, Florian and Dengler, Juergen (2008) GermanSL - eine universelle taxonomische Referenzliste für Vegetationsdatenbanken, Tuexenia, 28, 239-253.

Jansen, F., Dengler, J (in press) Plant names in vegetation databases - a neglected source of bias, 
Journal of vegetation science, \url{http://dx.doi.org/10.1111/j.1654-1103.2010.01209.x}
}

\keyword{ package }
