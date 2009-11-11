tv.eco <- function (db, eco = 'ecodbase', refl, tv_home)
{
    if(missing(tv_home)) tv_home <- tv.home()
    if(missing(refl))  refl <- tv.refl(db)
    eco <- read.dbf(paste(tv_home, 'Species', refl, paste(eco,'dbf',sep='.'), sep='/'))
    nofun <- function(x) all(is.na(x))
    na <- apply(eco, 2, nofun)
    if(any(na)) {print("The following columns contain no data and are omitted", quote=FALSE)
                print(names(eco)[na])
                eco <- eco[, !na]
                }
    emptyfun <- function(x) all(x == 0)
    empty <- apply(eco, 2, emptyfun)
    if(any(empty)) {
        print("The following columns contain only 0 values and are omitted", quote=FALSE)
        print(names(eco)[empty], quote=FALSE)
        eco <- eco[, !empty]
        }
    nullfun <- function(x) is.numeric(x) & any(x == 0, na.rm = TRUE)
    null <- logical()
    for (i in 1:length(eco)) null[i] <- nullfun(eco[, i])
    if(any(null)) {
        print("The following numeric fields contain 0 values.", quote=FALSE)
        print("Please check, if these are really measured as 0 or if they are wrongly assigned by Dbase.", quote=FALSE)
        print("If so, use something like eco$Column_name[eco$Column_name==0] <- NA", quote=FALSE)
        print(paste("summary(eco[,c('", paste(names(eco)[null],
            collapse = "','"), "')])", sep = ""))
        }
    print("Changing character fields into numericals:", quote=FALSE)
    for(i in 1:ncol(eco)) if(is.factor(eco[,i])) {
    print(paste(names(eco)[i], 'is factor'), quote=FALSE)
    eco[,i] <- as.character(eco[,i])
    eco[,i] <- type.convert(eco[,i]) }
    eco
}
