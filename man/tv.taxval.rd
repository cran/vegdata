\name{tv.taxval}
\alias{tv.taxval}

\title{Taxonomic emendation of vegetation data.}

\description{
Performs taxonomic valuation of species names according to synonomy, taxonomic level, unambiguity of biotic content etc.
Necessary prerequisite is information about taxonomic status (synonomy) and hierarchy (next higher aggregat).
 Until now only applicable for reference list 'GermanSL' applicable in Germany and adjacent countries.
}

\usage{
tv.taxval(db, obs, refl, tv_home, concept, syn = c('adapt','conflict','preserve'), subdiv = c('conflict', 'adapt', 'preserve'), ag = c('conflict', 'preserve', 'adapt'), mono = c('lower','higher', 'preserve'), monolist = "monotypic-D", genus = c('delete','preserve'), quiet = FALSE, sysPath = FALSE, ...)
}
\arguments{
\item{db}{a name of a Turboveg database directory containing \code{tvabund.dbf}, \code{tvhabita.dbf} and \code{twin.set}}
\item{obs}{data.frame of observations in TURBOVEG format, for example loaded with \code{\link{tv.obs}} }
\item{refl}{Name of taxonomic reference list}
\item{tv_home}{Path to TURBOVEG installation}
\item{concept}{Character vector calling the desired taxonomical concepts dataframe(s), see details}
\item{syn}{Treatment of synonyms, see details}
\item{subdiv}{Treatment of forms, variants, segregates and subspecies, see details}
\item{ag}{Treatment of aggregates between species and genus level, see details}
\item{mono}{Should monotypic taxa be combined at subspecies = \option{lower} or species level = \option{higher}}
\item{monolist}{Name of monotypic species list, must be in dbase format and in the same directory as the reference list, e.g. \code{"monotypic-D"} for the area of germany.}
\item{genus}{Delete or preserve taxa determined only on genus level.}
\item{quiet}{Suppress messages about changed names.}
\item{sysPath}{Usage of package system files or normal Turboveg installation files.}
\item{...}{Other parameters passed to functions.}
}

\details{
Working with vegetation datasets, especially from different sources needs taxonomic valuation. The function tries to automate this process. Therefore the German taxonomic reference list (GermanSL, \url{http://geobot.botanik.uni-greifswald.de/reflist/} ) contains additional lists for taxonomic attributes and monotypic taxa of Germany. Without an appropriate species list (see tax.dbf, \code{\link{tax}}) the function will not work.

Before we replace synonyms we can choose to evaluate the data under a different taxonomical concepts than the original GermanSL. In the moment only an outline of such an approach is realised, because complete lists of differing taxonyms and there taxonomical re-evaluation have to be provided which are not available. With the inbuild test-dataset and the interpretation of the Armeria maritima complex you can have a shot on applying different concepts using a dataset referenced with GermanSL and using \code{Korneck1996.dbf}. See package example session.

The three possible values for synonyms, variants, segregates and aggregates are: \code{preserve}: Leave everything untouched; \code{conflict}: Dissolve only in case of conflicts, e.g. if a subspecies occurrs also at the species level within the same dataset. In this case the subspecies will be aggregated to the species level. Otherwise it will stay untouched. \code{adapt}: All respective taxa will be adapted, e.g. set to species level.
  
Monotypic taxa, e.g. a species which occur only with 1 subspecies in the survey area. They have to be combined, since otherwise two different (valid) taxa would denominate the same entity. If lower the higher taxon (e.g. species rank) is replaced by the lower level (subspecies rank). If neither \code{lower} nor \code{higher} monotypic species are preserved. Since the list of monotypic species strongly depends on the considered area you have to choose, which area is covered by your database and create an appropriate list of monotypic taxa. Within the package \code{"monotypic-D.dbf"} is provided as a compilation of monotypic species within the GermanSL list (see \code{\link{tv.mono}}.

If \code{genus} is TRUE, all observations determined only to the species level are deleted.

Only the above mentioned types are embraced in this function. Others like forms are always synonyms in Standard lists.
}

\value{
Function returns a list of observations with valuated taxa numbers.
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }


\seealso{\code{\link{tv.veg}}, \code{\link{tv.obs}}}

\examples{
obs <- tv.taxval('taxatest', sysPath=TRUE)
## For explanations see vignette('vegdata').
}

\keyword{misc,manip}
