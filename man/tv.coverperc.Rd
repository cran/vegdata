\name{tv.coverperc}
\alias{tv.coverperc}
\title{Cover code translation}
\description{
Translate cover code into percentage cover values for Turboveg database observations.
}

\usage{
tv.coverperc(db, obs, RelScale, tv_home, tvscale, quiet = FALSE,  ...)
}

\arguments{
\item{db}{the name of the Turboveg database}
\item{obs}{dataframe of observations, containing Cover Codes, coded in tvscale.dbf of Turboveg installation}
\item{RelScale}{dataframe of CoverScale codes per releve, if empty it is read from the database}
\item{tv_home}{Path to Turboveg installation.}
\item{tvscale}{Cover scale.}
\item{quiet}{Suppress messages.}
\item{...}{Further options.}
}

\value{
  \item{obs}{data.frame of observations with additional column \code{COVER_PERC}}
}

\examples{
## For examples see in vignette('vegdata').
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }

\keyword{misc}