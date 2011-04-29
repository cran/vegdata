vw.biblio <- function(x, query, ...) {
  con <- vw.con()
  ref <- dbSendQuery(con, statement = paste(
    'SELECT b.*, p.*',
    'FROM beobachtung AS b LEFT JOIN plot AS p',
    'ON p.Plot_ID = b.Plotcode',
     paste('WHERE b.', query, sep='')
     ))
  res <- fetch(ref, n=-1)
 dbDisconnect(con)
  invisible(res)
} 

