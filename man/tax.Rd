\name{tax}
\alias{tax}
\alias{tax.default}
\alias{tax.veg}
\alias{childs}
\alias{child}
\alias{parents}
\alias{parent}
\alias{syn}

\title{Query of Turboveg 2 taxonomic reference lists including (if available) concept synonomy and taxonomic hierarchy.}

\usage{
\method{tax}{default}(x, refl, detailed = FALSE, syn = TRUE, concept = NULL, strict = FALSE, 
vernacular = FALSE, simplify = FALSE, quiet = FALSE, reflist.type = 'Turboveg', ...)
child(x, refl = tv.refl(), gen = 4, tree = FALSE, quiet = FALSE, syn = FALSE, ...)
parent(x, refl = tv.refl(), rank, quiet = FALSE, ...)
syn(x, refl = tv.refl(), quiet = FALSE, ...)
}

\arguments{
\item{x}{Species number, lettercode or species name}
\item{refl}{Taxonomic reference list}
\item{detailed}{Load tax.dbf with additional taxonomic information (e.g. Secundum) instead of species.dbf}
\item{syn}{Return also synonym names}
\item{concept}{Name of alternatice taxon view file within the reference list director}
\item{strict}{Exact match or partial matching with \code{\link{grep}}}
\item{vernacular}{Search in vernacular names instead of scientific names.}
\item{simplify}{Will simplify species names for matching.}
\item{gen}{Number of child generations to return}
\item{quiet}{Hide screen messages}
\item{reflist.type}{Type of taxonomic reference list to use. Until now only Turboveg lsits are suppported in the official package.}
\item{tree}{Opens a gWidgets window with interactive taxonomic tree view. Requires package gWidgets}
\item{rank}{Taxonomical level of parentship to find}
\item{...}{additional attributes}
}

\description{
Input is either species number (integer), shortletter (7 characters) or full (exact!) species name.
}

\details{
\dfn{concept}: GermanSL is a list with a single taxon view according to the standard lists of the different taxon groups (e.g Wisskirchen and Haeupler for higher plants, see). 
Nevertheless a hugh number of synonyms is included which allows in many cases the transformation into different concepts. 
For illustration the concept of \emph{Armeria maritima} from Korneck 1996 is included, which accepts e.g. \emph{Armeria maritima ssp. bottendorfensis}.
\dfn{simplify}: Before string comparison with reference list names it will eliminate diacritic marks, double consonants, "th", "y" and others. If genus=TRUE critical, i.e. non stable endings of genus names will be ignored, epithet = TRUE will eliminate specific endings for the last epithet.
}

\seealso{package vegdata}

\references{
Jansen, F. and Dengler, J. (2008) GermanSL - eine universelle taxonomische Referenzliste f\"ur Vegetationsdatenbanken. Tuexenia, 28, 239-253.
}

\examples{
\dontrun{
## GermanSL in Turboveg installation path needed
tax(27)
tax('Achillea millefolium')
tax('ACHIMILL')
}
\dontrun{
childs(0, gen=1)
childs(94419, tree=TRUE)
}
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
}
\keyword{misc}