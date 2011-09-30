\name{tv.write}
\alias{tv.write}
\title{Write species-plot observations and site information to Turboveg database.}

\description{Write species-plot observations and site information to Turboveg database.}

\usage{
tv.write(obs, site, name, cover = c("code", "perc"), overwrite = FALSE, ...)
}

\arguments{
  \item{obs}{Observations with RELEVE_NR, SPECIES_NR and COVER_CODE columns.}
  \item{site}{Header data for plots.}
  \item{name}{Name of the database.}
  \item{cover}{Use of covercodes or (mean) cover percentages, see Details.}
  \item{overwrite}{Logical. Should an existing database be overwritten.}
  \item{\dots}{Additional arguments.}
}

\details{
  By deafult covercode are written to Turboveg. This is only meaningful, if correct coverscales are stored in the site dataframe. In case of e.g. VegetWeb data it is better to choose percentage cover (and Cover code 00) for Turboveg).
}

\value{
  A dataframe of species occurences is written in dbase format into the Data directory of the specified Turboveg installation. Accordingly the header data information (tvhabita.dbf) is written to this directory and an empty file remarks.dbf is copied from the sample dataset.
}

\seealso{\code{\link{tv.veg}}}

\author{Florian Jansen \email{jansen@uni-greifswald.de}      }

\keyword{misc, survey}
