\name{tax}
\alias{tax}
\alias{spc}
\alias{agg}
\alias{childs}
\alias{parents}
\alias{syn}


\title{Query of taxonomic reference list including concept synonomy and taxonomic hierarchy.}

\usage{
tax(x, refl, verbose = FALSE, syn = TRUE, concept = NULL, strict = FALSE, ...)
childs(x, refl, species, gen = 4, tree = FALSE, quiet = FALSE, syn = FALSE, ...)
parents(x, refl, species, rank, ...)
syn(x, refl, species, quiet = FALSE, ...)
}

\arguments{
\item{x}{Species number, lettercode or species name}
\item{refl}{Taxonomic reference list}
\item{verbose}{Load tax.dbf with additional taxonomic information (e.g. Secundum) instead of species.dbf}
\item{syn}{Return also synonym names}
\item{concept}{Name of alternatice taxon view file within the reference list director}
\item{strict}{Exact match or partial matching with \code{\link{grep}}}
\item{species}{Taxonomic reference list (dataframe according to tax() output. If not given, tax(...) will be used}
\item{gen}{Number of child generations to return}
\item{quiet}{Hide screen messages}
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