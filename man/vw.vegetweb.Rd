\name{vegetweb}
\alias{vegetweb}
%\alias{vw.obs}
%\alias{vw.biblio}
\alias{vw.survey}
\alias{vw.veg}
\alias{vw.site}
\alias{vw.basket}

\title{Retrieve data from vegetweb}

\usage{
  vw.survey(searchstring, server, ...)
  vw.basket(user, password, server, ...)
  vw.site(user, password, id, server, ...)
  vw.veg(user, password, id, taxeval = TRUE, server, refl = "GermanSL 1.2", 
  ...)
}

\arguments{
  \item{user}{username}
  \item{password}{Password}
  \item{searchstring}{Part of the project title to select projects from the list of available vegetation surveys.}
  \item{id}{Identifier of the vegetweb project or data basket. Can be a survey id (see \code{vw.survey}), 
  or a data basket uuid (see \code{vw.basket} or part of the survey title (if it results in a single project)}
  \item{basket}{Shopping basket id of your vegetweb selection.}
  \item{taxeval}{Taxonomic harmonization of occurring taxa}
  \item{server}{server to address}
  \item{refl}{name of taxonomic reference list}
  \item{\dots}{additional arguments}
}

\description{VegetWeb is the national vegetation database of Germany, see \url{http://www.vegetweb.de}. Webservices are designed to retrieve data baskets, selected within the plattform. Access is granted through user accounts and individual clearances. vw.survey will search for projects (Publication titles with vegetation plots implemented in vegetweb.). vw.site retrieves plot header data and vw.veg the plant taxon observations in the usual plot x species matrix.}


\author{Florian Jansen \email{jansen@uni-greifswald.de} }

\examples{
\dontrun{
vw.survey('Leyer')
server <- 'botanik3.botanik.uni-greifswald.de'
veg <- vw.veg('info@vegetweb.de', 'vegetweb', 466, server = server)
site <- vw.site('info@vegetweb.de', 'vegetweb', 466, server = server)
}
}

\keyword{misc, survey}
