\encoding{UTF-8}
\name{vegdata-package}
\alias{vegdata-package}
\alias{vegdata}

\docType{package}
\title{
 Functions to access data from vegetation databases and evaluate taxon names.
}
\description{
This package provides a set of functions to load data from vegetation databases (at present Turboveg and vegetweb.de). 
Taxa can be (semi-)automatically be checked and adapted depending the scientific question. For this a hierachical taxonomic reference list is needed.
}

\details{
\tabular{ll}{
Package: \tab vegdata\cr
Type: \tab Package\cr
License: \tab GPL version 2 or newer\cr
LazyLoad: \tab yes\cr
}

Use \code{\link{tv.veg}} to prepare data directly for further analyses. Set option \code{\link{taxval}} to \code{TRUE}, if your database is referenced with GermanSL or equivalent taxonomic reference list and you want to realize taxonomic checks and adaptations.\\
For more details see \code{vignette('vegdata')}.
}
\author{
Florian Jansen

Maintainer: Florian Jansen <florian.jansen@uni-rostock.de>
}

\references{
Jansen, F., Dengler, J (2011) Plant names in vegetation databases - a neglected source of bias, 
Journal of vegetation science, 21(6), 1179-1186. \url{http://dx.doi.org/10.1111/j.1654-1103.2010.01209.x}

Jansen, Florian and Dengler, Juergen (2008) GermanSL - eine universelle taxonomische Referenzliste fuer Vegetationsdatenbanken, Tuexenia, 28, 239-253.
}

\keyword{package}
