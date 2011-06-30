\name{tax}
\alias{tax}
\alias{spc}

\title{Query of taxonomic reference list including concept synonomy.}

\usage{
tax(x, refl, verbose = FALSE, syn = FALSE, concept = NULL, sysPath = FALSE, ...)
}

\arguments{
\item{x}{Species number, lettercode or species name}
\item{refl}{Taxonomic reference list}
\item{verbose}{Load tax.dbf with additional taxonomic information (e.g. Secundum) instead of species.dbf}
\item{syn}{Prints also synonyms for shortletters.}
\item{concept}{Name of alternatice taxon view file within the reference list directory.}
\item{sysPath}{A small example reference list for dataset \code{elbaue} is available in the installation path of the package.}
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