\name{vegetweb}
\alias{vegetweb}
\alias{vw.obs}
\alias{vw.query}
\alias{vw.site}
\alias{vw.veg}
\alias{vw.biblio}

\title{Dataframe of plot-species observations from "VegetWeb"}

\description{VegetWeb is the national vegetation database of Germany, see \url{http://www.floraweb.de/vegetation/vegetweb} }

\usage{
  vw.obs(query= NULL, refl = "GermanSL 1.2", ...)
  vw.site(query=NULL, quiet=TRUE, ...)
  vw.veg(query, obs, taxval=TRUE, ...)
}

\arguments{
\item{query}{A SQL query for variables in table \code{beobachtung} as character string.}
\item{refl}{VegetWeb uses GermanSL 1.1}
\item{quiet}{Suppress messages about removed empty fields.}
\item{obs}{plot observations like output of \code{tv.obs()}}
\item{taxval}{apply taxonomic harmonisation? See \code{taxval}}
\item{\dots}{additional arguments}
}

\value{Data.frame of species occurences in dataframe format (like in Turboveg, see \code{tv.obs}).}

\seealso{\code{\link{vw.veg}}}

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
