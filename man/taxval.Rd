\name{taxval}
\alias{taxval}
\alias{tv.taxval}
\alias{comb.species}

\title{Handling of taxonomy in vegetation data.}

\description{
Performs taxonomic valuation of species names according to synonomy, taxonomic level, unambiguity of biotic content etc.
Necessary prerequisite is information about taxonomic status (synonomy) and hierarchy (next higher aggregat).
 Until now only applicable for reference list 'GermanSL' (>= vers. 1.1, see References Section), which is valid in Germany and adjacent countries.
}

\usage{
taxval(obs, refl, db, concept=NULL, syn = c('adapt','conflict','preserve'), 
ag = c('conflict', 'adapt', 'preserve'), 
rank, mono = c('species', 'higher', 'lower', 'preserve'), monolist = "monotypic-D", 
uncertain = NULL, maxtaxlevel = 'ROOT', check = TRUE, quiet = FALSE,  ...)
comb.species(x, sel, newname, refl)
}

\arguments{
  \item{obs}{data.frame of observations in TURBOVEG format, for example loaded with \code{\link{tv.obs}} }
  \item{refl}{Name of taxonomic reference list}
  \item{db}{a name of a Turboveg database directory containing \code{tvabund.dbf}, \code{tvhabita.dbf} and \code{twin.set}}
  \item{concept}{Character vector calling the desired taxonomical concepts dataframe(s), see details}
  \item{syn}{Treatment of synonyms, see details}
  \item{ag}{Treatment of childs and parents within the dataset, see details}
  \item{rank}{If ag='adapt', rank specifies the taxonomic rank to which taxa should be coarsened to. All higher taxa will be deleted.}
  \item{mono}{Should monotypic taxa be combined at subspecies = \option{lower} or species level = \option{higher}}
  \item{monolist}{Name of monotypic species list, must be in dbase format and in the same directory as the reference list, e.g. \code{"monotypic-D"} for the area of germany.}
  \item{uncertain}{List of length two, first the column name of uncertainty information, second a dataframe with uncertainty value and in column two one of 'delete','aggregate','preserve', see example.}
  \item{maxtaxlevel}{Maximum taxonomic levels to be used. See details.}
  \item{check}{Check for critical namesin your dataset and give warnings.'}
  \item{quiet}{Suppress messages about changed names.}
  \item{x}{Dataframe of class 'veg'. See \code{\link{tv.veg}}}
  \item{sel}{Vector of species (column names) to be combined.}
  \item{newname}{Name of the combined taxon.}
  \item{...}{Other parameters passed to functions.}
}

\details{
Working with vegetation datasets, especially from different sources needs taxonomic valuation. The function tries to automate this process. Therefore the German taxonomic reference list (GermanSL, \url{http://geobot.botanik.uni-greifswald.de/reflist/} ) contains additional taxon attributes (tax.dbf) and monotypic taxa of Germany (monotypic.dbf). Without an appropriate species list (see  \code{\link{tax}}) the function will not work.

Before we replace synonyms we can choose to evaluate the data under a different taxonomical concepts than the original GermanSL. In the moment only an outline of such an approach is realised, because complete lists of differing taxonyms and there taxonomical re-evaluation have to be provided which are not available. With the inbuild test-dataset and the interpretation of the Armeria maritima complex you can have a shot on applying different concepts using a dataset referenced with GermanSL and using \code{Korneck1996.dbf}. See package vignette.

The three possible values for synonyms and child/parent taxa are: \code{preserve}: Leave everything untouched; \code{conflict}: Dissolve only in case of conflicts, e.g. if a subspecies occurrs also at the species level within the same dataset. In this case the subspecies will be aggregated to the higher level. \code{adapt}: All respective taxa will be adapted, e.g. set to species level.
  
Monotypic taxa, e.g. a species which occur only with 1 subspecies in the survey area. They have to be combined, since otherwise two different (valid) taxa would denominate the same entity. If lower the higher taxon (e.g. species rank) is replaced by the lower level (subspecies rank). If neither \code{lower} nor \code{higher} monotypic species are preserved. Since the list of monotypic species strongly depends on the considered area you have to choose, which area is covered by your database and create an appropriate list of monotypic taxa. Within the package \code{"monotypic-D.dbf"} is provided as a compilation of monotypic species within the GermanSL list (see \code{\link{tv.mono}}.

Option maxtaxlevel determines the maximum taxonomic level within the given names, which should be used. All higher taxon observations are deleted. If you have a single field observation determined as \emph{Asteraceae spec.} all your obervations of taxa from that family will be aggregated to the family level, if you choose ag=conflict.
}

\value{
Functions return the input dataframe of observations with harmonised taxon numbers.
}


\seealso{\code{\link{tv.veg}}, \code{\link{tv.obs}}}

\references{
Jansen, F. and Dengler, J. (2008) GermanSL - eine universelle taxonomische Referenzliste f\"ur Vegetationsdatenbanken. Tuexenia, 28, 239-253.
Jansen, F. and Dengler, J. (2010) Plant names in vegetation databases - a neglected source of bias. Journal of Vegetation Science, 21, 1179-1186.
}

\author{Florian Jansen
  \email{jansen@uni-greifswald.de}
 }

\examples{
\dontrun{
# Turboveg installation needed
obs <- taxval(db='taxatest')
## For explanations see vignette('vegdata').

veg <- tv.veg('taxatest')
veg <- comb.species(veg, c('ARMEM-E','ARMEM-H'))
}
}

\keyword{misc,manip}
