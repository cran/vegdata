\name{monotypic}
\alias{monotypic}
\alias{tv.mono}
\title{Creates a list of (regional) monotypic taxa from the given taxonomic checklist}

\usage{
monotypic(refl, nr.member = 1, reflist.type = c('Turboveg', 'EDIT'), write = FALSE, 
filename, tv_home, ...)
}

\arguments{
\item{refl}{The name of the taxonomic reference list.}
\item{nr.member}{Number of members in the next taxonomic level to be checked.}
\item{reflist.type}{Type or origin of the taxonomic list: Turboveg 2.0 format or from the European Distributed Institut of Taxonomy.}
\item{write}{Should the list of monotypic species be written into a CSV file for further use.}
\item{filename}{Name of the file in case of write=TRUE}
\item{tv_home}{Turboveg installation path, see \link{tv.home} }
\item{...}{additional arguments}
}

\description{
More or less internal function to check the reference lists and to create lists of monotypic taxa.
}

\value{
Dataframe of monotypic taxa.
}

\author{Florian Jansen
\email{florian.jansen@uni-rostock.de}
        }

\keyword{misc}