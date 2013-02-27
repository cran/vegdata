\name{tv.mono}
\alias{tv.mono}
\title{Creates a list of monotypic taxa from the Taxonomic Reference List}

\usage{
tv.mono(refl, write = FALSE, filename = "monotypic-D.dbf", nr.member = 1, tv_home, ...)
}

\arguments{
\item{refl}{The name of the taxonomic reference list.}
\item{write}{Should the list of monotypic species be written into a DBase file for further use.}
\item{filename}{Name of the file in case of write=TRUE}
\item{nr.member}{Number of members in the next taxonomic level to be checked.}
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
\email{jansen@uni-greifswald.de}
        }

\keyword{misc}