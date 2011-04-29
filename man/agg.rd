\name{childs}
\alias{childs}
\alias{parents}
\alias{agg}

\title{Show childs or parents of specified taxa}

\description{
Show all lower respectively higher taxonomic ranks of specified taxa.
}

\usage{
childs(x, refl, species, gen = NULL, quiet=FALSE, ...)
parents(x, refl, species, quiet=FALSE, ...)
}

\arguments{
\item{x}{Taxon number, name or shortletter.}
\item{refl}{Taxonomic reference list to use. If not given, default GermanSL version will be chosen.}
\item{species}{Output of \code{tax('all', ...)}. If not given, Turboveg reference list will be used.}
\item{gen}{Number of child generations to return, defaults to all.}
\item{quiet}{Hide results on screen.}
\item{...}{additional arguments, e.g. for function \code{tax}}
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }

\keyword{misc}