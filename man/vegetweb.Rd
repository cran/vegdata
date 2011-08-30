\name{vegetweb}
\alias{vegetweb}
\alias{vw.obs}
\alias{vw.query}
\alias{vw.site}
\alias{vw.veg}
\alias{vw.biblio}

\title{Vegetation data from "VegetWeb"}

\description{VegetWeb is the national vegetation database of Germany, see \url{http://www.floraweb.de/vegetation/vegetweb} }

\usage{
  vw.obs(query= NULL, refl = "GermanSL 1.2", ...)
  vw.site(query=NULL, quiet=TRUE, ...)
  vw.veg(query, obs, taxval=TRUE, ...)
}

\arguments{
\item{query}{A SQL query for variables in VegetWeb table \code{beobachtung} as character string, see example.}
\item{refl}{VegetWeb uses GermanSL 1.1}
\item{quiet}{Suppress messages about removed empty fields.}
\item{obs}{dataframe with plot-species observations, see \code{\link{tv.obs}}}
\item{taxval}{apply taxonomic harmonisation? See \code{\link{taxval}}}
\item{\dots}{additional arguments}
}

\examples{
\dontrun{
  vw.site()
  con <- vw.con()
  dbGetQuery(con, "SELECT * FROM projekt")[1:2]

  query <- "Projekt='T255'"
  site <- vw.site(query)
  names(site)
  obs <- vw.obs(query)
  names(obs)
}
}

\author{Florian Jansen \email{jansen@uni-greifswald.de} }

\keyword{misc, survey}
