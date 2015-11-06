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

\description{VegetWeb is the national vegetation database of Germany, see \url{http://www.vegetweb.de} }


\author{Florian Jansen \email{jansen@uni-greifswald.de} }

\keyword{misc, survey}
