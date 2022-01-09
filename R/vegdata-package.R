#' @title vegdata
#' @description Functions to access data from vegetation databases and evaluate taxon names.
#'
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom curl curl_download
#' @importFrom magrittr %>%
#' @importFrom foreign read.dbf
#' @importFrom xml2 read_xml xml_find_all xml_attr xml_attrs xml_length xml_children
#' @importFrom utils find read.csv download.file head tail type.convert unzip read.csv2 write.csv2
#' @importFrom plyr ldply
#' @importFrom stats na.omit
#'
#' @name vegdata-package
#' @aliases vegdata
#' @docType package
#' @keywords package
#'
#' @encoding UTF-8
#' @docType package
#'
#' @section Introduction:
#' This package provides a set of functions to load data from vegetation databases (at present Turboveg and vegetweb.de).
#' Taxa can be (semi-)automatically be checked and adapted depending the scientific question. For this a hierachical taxonomic reference list is needed.
#'
#' @section Taxonomic harmonization:
#' Use \code{\link{tv.veg}} to prepare data directly for further analyses. Set option \code{ßlink{taxval}} to \code{TRUE}, if your database is referenced with GermanSL or equivalent taxonomic reference list and you want to realize taxonomic checks and adaptations.
#' For more details see \code{vignette('vegdata')}.
#'
#' @author Florian Jansen \email{florian.jansen@uni-Rostock.de}
#'
#' @references
#' Jansen, F., Dengler, J (2011) Plant names in vegetation databases - a neglected source of bias,
#' Journal of vegetation science, 21(6), 1179-1186. @url{http://dx.doi.org/10.1111/j.1654-1103.2010.01209.x}
#'
#' Jansen, Florian and Dengler, Juergen (2008) GermanSL - eine universelle taxonomische Referenzliste fuer Vegetationsdatenbanken, Tuexenia, 28, 239-253.
#'
# Needed for use of . in magrittr pipelines
utils::globalVariables(c(".", "multipatt", "write.dbf","gwindow", "gtree", "addHandlerDoubleclick", "svalue"))

NULL
