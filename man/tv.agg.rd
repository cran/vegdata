\name{tv.agg}
\alias{tv.agg}
\alias{tv.member}

\title{Show or save members of taxonomic level}

\description{
Show the members of taxonomical levels, e.g. aggregates \code{AGG} as a list (and save it into a file).
}

\usage{
tv.agg(nr.member='ALL', refl, tv_home, agg=c('AGG','SER','SEC','SSE','SGE','SGR'), ...)
tv.member(x, tv_home, refl='GermanSL 1.1', syn = FALSE, ...)
}

\arguments{
\item{x}{Species number, lettercode or name}
\item{refl}{Taxonomic reference list to use}
\item{tv_home}{Directory path of Turbowin installation}
\item{agg}{Taxonomic levels to check}
\item{nr.member}{the numbers of member for which aggregates should be displayed, 1 shows only aggregates with a single member (monotypic species), 'ALL' for all}
\item{syn}{incorporate synonyms}
\item{...}{additional arguments}
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }

\keyword{misc}