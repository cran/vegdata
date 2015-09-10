\name{tv.write}
\alias{tv.write}
\title{Write species-plot observations and site information to Turboveg database.}

\description{Write species-plot observations and site information to Turboveg database.}

\usage{
tv.write(x, site, db, name, cover = c("code", "perc"), overwrite = FALSE, 
iconv="CP437", newTvAdmin = FALSE, ...)
}

\arguments{
  \item{x}{Either observations data.frame with RELEVE_NR, TaxonUsageID and COVER_CODE columns or vegetation matrix of class "veg".}
  \item{site}{Header data for plots.}
  \item{db}{name of original Turboveg database folder}
  \item{name}{Name of the new database.}
  \item{cover}{Use of covercodes or (mean) cover percentages, see Details.}
  \item{overwrite}{Logical. Should an existing database be overwritten.}
  \item{iconv}{specify locale if necessary}
  \item{newTvAdmin}{Build new TvAdmin.dbf? If FALSE the file from original database will be used.}
  \item{\dots}{Additional arguments.}
}

\details{
  By default Covercode is written to Turboveg. This is only meaningful, if correct CoverScales are stored in the site dataframe.
  Database name is iportent to care for TvAdmin.dbf and remarks.dbf and configuration file tvwin.set.
}

\value{
  A dataframe of species occurences is written in dbase format into the Data directory of the specified Turboveg installation. Accordingly the header data information (tvhabita.dbf) is written to this directory and an empty file remarks.dbf is copied from the sample dataset.
}

\seealso{\code{\link{tv.veg}}}

\author{Florian Jansen \email{jansen@uni-greifswald.de}      }

\keyword{misc, survey}
