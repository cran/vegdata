\name{tax}
\alias{tax}
\alias{tax.default}
\alias{tax.veg}
\alias{childs}
\alias{child}
\alias{parents}
\alias{parent}
\alias{syn}
\alias{taxname.abbr}
\alias{taxname.simplify}
\alias{parse.taxa}
\alias{taxname.removeAuthors}

\title{Query of Turboveg 2 taxonomic reference lists including (if available) concept synonomy and taxonomic hierarchy.}

\usage{
\method{tax}{default}(x, refl, detailed = FALSE, syn = TRUE, concept = NULL, strict = FALSE, 
simplify = FALSE, quiet = FALSE, reflist.type = 'Turboveg', ...)
child(x, refl = tv.refl(), gen = 4, quiet = FALSE, syn = FALSE, ...)
parent(x, refl = tv.refl(), rank, quiet = FALSE, ...)
syn(x, refl = tv.refl(), quiet = FALSE, ...)
taxname.abbr(x, hybrid = c('remove', 'retain'), 
species = FALSE, cf = FALSE, ...)
taxname.simplify(x, genus=TRUE, epithet=TRUE,  hybrid = c("remove", "retain"), 
        rank = c("remove", "retain"), concept.status = c("remove", "retain"), ...)
parse.taxa(x, epis)
taxname.removeAuthors(x)
}

\arguments{
\item{x}{Species number, lettercode or species name(s)}
\item{refl}{Taxonomic reference list}
\item{detailed}{In old Turboveg versions detailed taxonomic information could only be given in an extra file which was called tax.dbf in GermanSL. Compatibility mode.}
\item{syn}{Return also synonym names}
\item{concept}{Name of the file with an alternative taxon view stored in the reference list directory, see details.}
\item{strict}{Exact match or partial matching with \code{\link{grep}}}
\item{simplify}{Will simplify species names for matching.}
\item{gen}{Number of child generations to return}
\item{quiet}{Hide screen messages}
\item{reflist.type}{Type of taxonomic reference list to use. Until now only Turboveg lists are supported in the official package.}
\item{rank}{Taxonomical level of taxa to find}
\item{hybrid}{remove hybrid markers for comparisons}
\item{species}{use spec., sp., or species for genus level taxa}
\item{cf}{remove 'in doubt' marker}
\item{genus}{simplify genus name part}
\item{epithet}{simplify epithet(s)}
\item{concept.status}{remove s. str. s. l.}
\item{epis}{vector of rank marker strings, if missing: 'subsp.', 'var.', 'v.'}
\item{...}{additional attributes}
}

\description{
Input is either species number (integer), shortletter (7 characters) or full (exact!) species name.
}

\details{
\dfn{concept}: GermanSL is a list with a single taxon view according to the standard lists of the different taxon groups (e.g Wisskirchen and Haeupler for higher plants, see). 
Nevertheless a huge number of synonyms is included which allows in many cases the transformation into different concepts. 
For illustration the concept of \emph{Armeria maritima} from Korneck 1996 is included, which accepts e.g. \emph{Armeria maritima ssp. bottendorfensis}.

\dfn{taxname.simplify}: Before string comparison with reference list names it will eliminate diacritic marks, double consonants, "th", "y" versus "i(i)" and other frequent differences in writing style. If genus = TRUE (non stable) endings of genus names will be ignored, epithet = TRUE will eliminate endings for the epithet part.

\dfn{taxname.abbr}: standardisation of rank names

\dfn{parse.taxa}: parse genus and epitheta from name strings.

\dfn{taxname.removeAuthors} Remove name authors from full scientific name strings.
}

\seealso{package vegdata}

\references{
Jansen, F. and Dengler, J. (2008) GermanSL - eine universelle taxonomische Referenzliste f\"ur Vegetationsdatenbanken. Tuexenia, 28, 239-253.
}

\examples{
taxname.abbr('Achillea millefolium s. millefolium')
taxname.simplify('Vicia sylvatica') == taxname.simplify('Vicia silvaticum')


\dontrun{
## GermanSL in Turboveg installation path needed
tax(27)
tax('Achillea millefolium')
tax('ACHIMILL')
}
\dontrun{
child(0, gen=1)
child(27)
}
}

\author{Florian Jansen
\email{florian.jansen@uni-rostock.de}
}

\keyword{misc}