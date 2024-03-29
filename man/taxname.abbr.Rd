% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tax.names.R
\name{taxname.abbr}
\alias{taxname.abbr}
\title{Standardisation of taxonomic names, especially taxon rank indicators and hybrid signs
name taxname.abbr}
\usage{
taxname.abbr(x, hybrid = TRUE, concept = FALSE, species = TRUE, cf = TRUE, ...)
}
\arguments{
\item{x}{(integer or character) Species number, lettercode or species name(s)}

\item{hybrid}{(logical) remove hybrid markers for comparisons}

\item{concept}{(logical) remove concept additions like "s. str.", "s. l.}

\item{species}{(logical) remove "spec.", "sp.", or "species" for genus level taxa}

\item{cf}{(logical) remove 'in doubt' marker}

\item{\dots}{additional attributes}
}
\description{
Standardisation of taxonomic names, especially taxon rank indicators and hybrid signs
name taxname.abbr
}
\author{
Florian Jansen florian.jansen@uni-rostock.de
}
