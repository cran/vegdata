\name{tv.write}
\alias{tv.write}
\title{Write species-plot observations and site information to Turboveg database.}

\description{Write species-plot observations and site information to Turboveg database.}

\usage{
tv.write(x, site, name, tvadmin, remarks, dict = "", cover = c("code", "perc"), 
drop = FALSE, obl = TRUE, overwrite = FALSE, iconv="CP437", ...)
}

\arguments{
  \item{x}{Either observations data.frame with RELEVE_NR, TaxonUsageID and COVER_CODE columns or vegetation matrix of class "veg".}
  \item{site}{Header data for plots.}
  \item{name}{Name of the new database.}
  \item{tvadmin}{Dataframe with plot UUID's and Turboveg columns from TvAdmin.dbf. 
  A new file with new unique identifiers will be created if omitted.}
  \item{remarks}{Remarks in Turboveg format if the comments for individual plots exceed 254 characters. See remarks.dbf in Turboveg databases. An empty file will be created if omitted.}
  \item{dict}{Turboveg dictionary name}
  \item{cover}{Use of covercodes or (mean) cover percentages, see Details.}
  \item{drop}{Drop columns which are empty or contain only NA values.}
  \item{obl}{Add obligatory fields defined in the TV dictionary but not present in the site data table.}
  \item{overwrite}{Logical. Should an existing database be overwritten.}
  \item{iconv}{specify locale if necessary}
  \item{\dots}{Additional arguments.}
}

\details{
  By default Covercode is written to Turboveg. This is only meaningful, if correct CoverScales are given in the site dataframe.
  Unique plot ID's are stored in TvAdmin.dbf. If you want to preserve already given UUID's you have to prepare an adequate data.frame. Look into existing TvAdmin.dbf files for necessary columns.
}

\value{
  Five files will be created in tv_home/Data/<name> directory. 
  tvabund.dbf with occurrence information n long format, 
  tvhabita.dbf with plot information, remarks.dbf with comments longer then 255 characters, 
  TvAdmin.dbf with plot UUID's and tvwin.dbf with information about taxonomic reference list, 
  and dictionary used.
}

\seealso{\code{\link{tv.veg}}}

\author{Florian Jansen \email{florian.jansen@uni-rostock.de}}

\keyword{misc, survey}
