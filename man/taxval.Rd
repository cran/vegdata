% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxval.r
\name{taxval}
\alias{taxval}
\title{Handling of taxonomy in vegetation data.}
\usage{
taxval(obs, refl, db, ag = c('conflict', 'adapt', 'preserve'), rank,
mono = c('species', 'higher', 'lower', 'preserve'), monolist = "monotypic-D",
maxtaxlevel = 'AGG', check.critical = TRUE, interactive = FALSE, ...)
}
\arguments{
\item{obs}{data.frame of observations in TURBOVEG format, for example loaded with \code{\link{tv.obs}}}

\item{refl}{Name of taxonomic reference list}

\item{db}{a name of a Turboveg database directory containing \code{tvabund.dbf}, \code{tvhabita.dbf} and \code{twin.set}}

\item{ag}{Treatment of children and parents within the dataset, see details}

\item{rank}{If ag='adapt', rank specifies the taxonomic rank to which taxa should be coarsened to. All higher taxa in this taxonomic tree will be deleted, see maxtaxlevel.}

\item{mono}{Should monotypic taxa be combined at subspecies = \option{lower} or species level = \option{higher}}

\item{monolist}{Name of monotypic species list, must be in dBase format and in the same directory as the reference list, e.g. \code{"monotypic-D"} for the area of Germany.}

\item{maxtaxlevel}{Maximum taxonomic levels to be used. See details.}

\item{check.critical}{Check for critical names in your dataset and give warnings.'}

\item{interactive}{Do you want to adapt the list of changes.}

\item{...}{Other parameters passed to functions.}
}
\value{
Functions return the input dataframe of observations with harmonised taxon numbers.
}
\description{
Performs taxonomic valuation of species names according to synonomy, taxonomic level, unambiguous biotic content etc.
  Necessary prerequisite is information about taxonomic status (synonomy) and hierarchy (next higher aggregate).
  Until now only applicable for reference list 'GermanSL' (>= version 1.1, see References Section), which is valid in Germany and adjacent countries.
}
\details{
Working with vegetation datasets, especially from different sources needs taxonomic valuation. The function tries to automate this process. Therefore the German taxonomic reference list (GermanSL, \url{https://germansl.infinitenature.org} ) contains additional taxon attributes (tax.dbf) and monotypic taxa of Germany (monotypic.dbf). Without an appropriate species list (see \code{\link{tax}}) the function will not work.

The taxonomic reference list needs Taxonrank corresponding to values given in  taxlevels
Possible values for adapting the taxonomic hierarchy within the dataset (child/parent taxa) are: \code{preserve}: Leave everything untouched. \code{conflict}: Dissolve only in case of conflicts, e.g. if a subspecies occurs also at the species level within the same dataset. In this case the subspecies will be aggregated to the higher level. \code{adapt}: Dissolve all nested taxa to e.g. species level for option ag. For this option also option \code{rank}, specifying the rank to which the taxa shall be adapted, must be given.

Monotypic taxa, e.g. a species which occur only with 1 subspecies in the survey area. They have to be combined, since otherwise two different (valid) taxa would denominate the same entity. If lower the higher taxon (e.g. species rank) is replaced by the lower level (subspecies rank). If neither \code{lower} nor \code{higher} monotypic species are preserved. Since the list of monotypic species strongly depends on the considered area you have to choose, which area is covered by your database and create an appropriate list of monotypic taxa. Within the package \code{"monotypic-D.csv"} is provided as a compilation of monotypic species within the GermanSL list.

Option maxtaxlevel determines the maximum taxonomic level within the given names, which should be used. All higher taxon observations are deleted. If you have a single field observation determined as \emph{Asteraceae spec.} all your obervations of taxa from that family will be aggregated to the family level, if you choose ag=conflict.

\code{Interactive} If you want to manually adapt the taxonomic harmonization \code{interactive=TRUE} will create a table with all original names and NewTaxonID's according to the chosen rules. The table will be saved as \code{taxvalDecisionTable.csv} in your actual working directory. You can manipulate the column NewTaxonID. If you run \code{taxval} again (e.g. through function \code{\link{tv.veg}}) and a file with this name exist in your working directory, it will be used.
}
\examples{
\dontrun{
# Turboveg installation needed
 obs <- taxval(db='taxatest')
# For explanations see vignette('vegdata').

 veg <- tv.veg('taxatest')
 veg <- comb.species(veg, c('ARMEM-E','ARMEM-H'))
}

}
\references{
Jansen, F. and Dengler, J. (2008) GermanSL - eine universelle taxonomische Referenzliste f\"ur Vegetationsdatenbanken. Tuexenia, 28, 239-253.
Jansen, F. and Dengler, J. (2010) Plant names in vegetation databases - a neglected source of bias. Journal of Vegetation Science, 21, 1179-1186.
}
\seealso{
tv.veg, tv.obs
}
\author{
Florian Jansen \email{florian.jansen@uni-rostock.de}
}
