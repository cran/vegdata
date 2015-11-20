\name{vegetweb}
\alias{vegetweb}
%\alias{vw.obs}
%\alias{vw.biblio}
\alias{vw.survey}
\alias{vw.veg}
\alias{vw.site}

\title{Retrieve data from vegetweb}

\usage{
  vw.survey(searchstring, ...)
  vw.site(user, password, survey, basket, ...)
  vw.veg(user, password, survey, basket, taxeval = TRUE, ...)
}

\arguments{
  \item{user}{username}
  \item{password}{Password}
  \item{searchstring}{Part of the project title to select projects from the list of available vegetation surveys.}
  \item{survey}{Identifier of the vegetweb project.}
  \item{basket}{Shopping basket id of your vegetweb selection.}
  \item{taxeval}{Taxonomic harmonization of occurring taxa}
  \item{\dots}{additional arguments}
}

\description{VegetWeb is the national vegetation database of Germany, see \url{http://www.vegetweb.de}. Webservices are designed to retrieve data baskets, selected within the plattform. Access is granted through user accounts and individual clearances. vw.survey will search for projects (Publication titles with vegetation plots implemented in vegetweb.). vw.site retrieves plot header data and vw.veg the plant taxon observations in the usual plot x species matrix.}


\author{Florian Jansen \email{jansen@uni-greifswald.de} }

\examples{
\dontrun{
vw.survey('Leyer')
veg <- vw.veg('info@vegetweb.de', 'vegetweb', 466)
site <- vw.site('info@vegetweb.de', 'vegetweb', 466)
library(rgeos)
coord <- SpatialMultiPointsDataFrame(sapply(site$geom, readWKT), data.frame(a=1:nrow(site)))
plot(coord, axes = TRUE)
}
}

\keyword{misc, survey}
