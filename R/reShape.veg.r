reShape.veg <- function (veg, crop = TRUE, refl) {
if(!'veg' %in% class(veg)) stop('Only applicable for objects of class \"veg\".')
if(is.null(attr(veg, 'taxreflist')) & missing(refl)) stop('Set option refl because attribute \"taxreflist\" is not set for object \"veg\".')
  plots <- as.integer(as.character(dimnames(veg)[[1]][row(veg)]))
  veg <- as.matrix(veg)
  perf <- as.vector(veg)
  spec <- dimnames(veg)[[2]][col(veg)]
  spcnames <- sapply(spec, function(x) strsplit(as.character(x), ".", fixed = TRUE))
  layer <- as.integer(lapply(spcnames, function(x) x[2]))
  code <- unlist(lapply(spcnames, function(x) x[1]))
  code <- type.convert(code)
  if(is.character(code)) {
    if(all(sapply(code, nchar) == 7)) {
      TaxonUsageID <- integer(length(code))
      taxa <- tax(code, syn = FALSE)
      TaxonUsageID <- taxa$TaxonUsageID[match(code, taxa$LETTERCODE)]
    } else {
      TaxonUsageID <- integer(length(code))
      taxa <- tax(code, syn = FALSE)
      TaxonUsageID <- taxa$TaxonUsageID[match(code, taxa$TaxonName)]
    }
  } else TaxonUsageID <- as.integer(code)
  df <- data.frame(RELEVE_NR = plots, SPECIES_NR = TaxonUsageID, 
                   COVER_CODE = as.character(perf), LAYER = layer, stringsAsFactors = FALSE)
  df <- df[order(df$RELEVE_NR, df$SPECIES_NR), ]
  df <- df[df$COVER_CODE != 0 & !is.na(df$COVER_CODE), ]
  class(df) <- c("tv.obs", "data.frame")
  if (!is.null(attr(veg, "taxreflist"))) 
    attr(df, "taxreflist") <- attr(veg, "taxreflist")
  return(df)
}
