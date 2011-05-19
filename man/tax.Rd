\name{tax}
\alias{tax}
\alias{spc}

\title{Query of taxonomic reference list including concept synonomy.}

\usage{
tax(x, refl, syn = FALSE, tax = FALSE, concept = NULL, ...)
}

\arguments{
\item{x}{Species number, lettercode or species name}
\item{refl}{Taxonomic reference list}
\item{syn}{Prints also synonyms for shortletters.}
\item{tax}{Load tax.dbf instead of species.dbf}
\item{concept}{Name of alternatice taxon view file in reference list directory}
\item{...}{Other attributes}
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
## Turboveg installation needed
tax(27)
tax('Achillea millefolium')
tax('ACHIMILL')
}
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }

\keyword{misc}