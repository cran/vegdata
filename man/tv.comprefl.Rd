\name{tv.compRefl}

\alias{tv.compRefl}

\title{
Compare different taxonomical reference lists.}

\description{
The function checks for different taxon numbers and, or taxon names in two TURBOVEG reference lists.}

\usage{
   tv.compRefl(refl1, refl2, tv_home, check.nr=FALSE, 
   simplify = TRUE, verbose=FALSE, Sink=TRUE, 
   new = FALSE, file="compRefl.txt", ...)
}

\arguments{
  \item{refl1}{First reference list to compare.}
  \item{refl2}{Second reference list to compare.}
  \item{tv_home}{TURBOVEG installation path. If not specified, guessed by code{tv.home} }
  \item{check.nr}{Check equality of species numbers.}
  \item{simplify}{normalize taxon names with function taxname.simplify}
  \item{verbose}{Print species names on screen.}
  \item{Sink}{Write text file with differences.}
  \item{new}{Write new combined TURBOVEG reference list.}
  \item{file}{Name of the sink file.}
  \item{\dots}{Additional arguments.}
}

\references{
Jansen, F. and Dengler, J. (2010) Plant names in vegetation databases - a neglected source of bias. Journal of Vegetation Science, 21, 1179-1186.}

\author{Florian Jansen \email{florian.jansen@uni-rostock.de} }

\seealso{
\code{\link{tax}}
}

\keyword{utilities}
