#' Converst to a weekly resolution from a daily one on a financial data table
#' @import data.table
#' @param dt daily resolution data.table
#' @export
DayToWeek <- function(dt) {
  
  . <- .N <- Close <- Close1 <- Date <- High <- Low <- Open <- Open1 <- asdate <- diffday <- isoweek <- NULL
  lagoneday <- maxweek <- minweek <- shift <- week <- year <-  NULL
  
  dt <- setDT(dt)
  dt[,asdate:=as.Date(Date)]
  dt[,lagoneday := shift(asdate)]
  dt[!is.na(lagoneday), diffday := abs(as.numeric(asdate-lagoneday))]
  if (unique(dt[!is.na(diffday), .(diffday)])[,.N] > 1 | unique(dt[!is.na(diffday), .(diffday)])[,.N] != 1){
    stop("Something's wrong with the dates ")
  }
  
  dt[,`:=`(
    lagoneday = NULL,
    diffday = NULL,
    week = paste0(year(asdate), isoweek(asdate))
  )]
  dt[,`:=`(
    minweek = min(asdate),
    maxweek = max(asdate)
  ), by = .(week)]
  
  dt[,c("High", "Low") := {
    len <- length(.(Open))
    allvalues <- rbind(o = Open,
                       c = Close,
                       h = High,
                       l = Low)
    H <- max(allvalues)
    L <- min(allvalues)
    .(rep(H, len), rep(L, len))
  }, by = .(week)]
  
  dt[, Open1 := ifelse(asdate == minweek,1,0)]
  dt[, Close1 := ifelse(asdate == maxweek,1,0)]
  dt[,c("Open", "Close") := (
    .(sum(Open*Open1),
    sum(Close*Close1)
    )
  ), by = .(week)]
  
  return(unique(dt[,.(Date = maxweek,
                      Open, Close, High, Low)]))
  
}
