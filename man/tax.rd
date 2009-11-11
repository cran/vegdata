\name{tax}
\alias{tax}
\alias{spc}

\title{Query of taxonomic refernce list including concept synonomy.}

\usage{
tax(x, refl='Germansl 1.1', tv_home, syn = FALSE, tax = FALSE, concept = NULL, ...)
}

\arguments{
\item{x}{Species number, lettercode or species name}
\item{tv_home}{Path to Turboveg installation}
\item{refl}{Taxonomic reference list}
\item{syn}{Print also possible synonyms for shortletters.}
\item{tax}{Load tax.dbf instead of species.dbf}
\item{concept}{Name of alternatice taxon view file in refrence list directory}
\item{...}{Other attributes}
}

\description{
Input is either species number, shortletter (7 characters) or full (exact!) species name.
}

\details{
\dfn{concept}: GermanSL i a list with a single taxon view according to the standard list of the different group (e.g Wisskirchen and Haeupler for higher plants). 
Nevertheless a hugh number of synonyms is included which allows in many cases the transformation into different concepts. 
For illustration the concept of \emph{Armeria maritima} from Korneck 1996 is included, which accepts e.g. \emph{Armeria maritima ssp. bottendorfensis}.
}

\seealso{\code{\link{tv.taxval}}}

\examples{
\dontrun{
tax{27} }
tax(27, sysPath=TRUE)
tax(20583, tax=TRUE, sysPath=TRUE)
tax(20583, concept='korneck1996', sysPath=TRUE)
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }

\keyword{misc}