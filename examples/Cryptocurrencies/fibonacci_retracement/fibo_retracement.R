
library(data.table)

###   OPTIONS
from <- fasttime::fastPOSIXct("2020-11-28")
to <- fasttime::fastPOSIXct("2021-02-28")
fib_numbers <- c(0, 0.236, 0.382, 0.5, 0.681, 1)
################################################


dt <- fread(paste0(examples_path, "/Cryptocurrencies/data/coin_Bitcoin.csv"))
dt[,Date := fasttime::fastPOSIXct(Date)]
dt <- dt[Date >= from & Date <= to]
dt[,`:=`(
  MAX = max(High),
  MIN = min(Low)
)]
dt[MAX == High, max_timeframe := 1]
dt[MIN == Low, min_timeframe := 1]

# In case that max (min) appears multiple times
# we will consider max (min) as the latest max (min)
if (dt[max_timeframe == 1,.N] > 1){
  dt[max_timeframe == 1, tmp_timeframe := Date]
  dt[,tmp_timeframe := min(tmp_timeframe, na.rm = T)]
  dt[,max_timeframe := ifelse(tmp_timeframe == Date, max_timeframe, NA)]
  dt[,tmp_timeframe := NULL]
}
if (dt[min_timeframe == 1,.N] > 1){
  dt[min_timeframe == 1, tmp_timeframe := Date]
  dt[,tmp_timeframe := min(tmp_timeframe, na.rm = T)]
  dt[,min_timeframe := ifelse(tmp_timeframe == Date, min_timeframe, NA)]
  dt[,tmp_timeframe := NULL]
}

if (dt[max_timeframe == 1,Date] > dt[min_timeframe == 1, Date]){
  inv_normalized <- function(x){
    x_max <- dt[1,MAX]
    x_min <- dt[1,MIN]
    return(-x * (x_max - x_min) + x_max)
  }
} else {
  inv_normalized <- function(x){
    x_max <- dt[1,MAX]
    x_min <- dt[1,MIN]
    return(x * (x_max - x_min) - x_min)
  }
}

fib_retracement <- inv_normalized(fib_numbers)
dt[, X1 := fib_retracement[1]]
dt[, X2 := fib_retracement[2]]
dt[, X3 := fib_retracement[3]]
dt[, X4 := fib_retracement[4]]
dt[, X5 := fib_retracement[5]]
dt[, X6 := fib_retracement[6]]

p <- plot_ly(
  dt ,
  x = ~Date, type = "candlestick", open = ~Open,
  close = ~Close, high = ~High, low = ~Low
)
p <- add_lines(p, x = ~Date, y = ~X1, inherit = F)
p <- add_lines(p, x = ~Date, y = ~X2, inherit = F)
p <- add_lines(p, x = ~Date, y = ~X3, inherit = F)
p <- add_lines(p, x = ~Date, y = ~X4, inherit = F)
p <- add_lines(p, x = ~Date, y = ~X5, inherit = F)
p <- add_lines(p, x = ~Date, y = ~X6, inherit = F)
p
p <- layout(p, showlegend = FALSE, title = "BTC/USD", yaxis = "")
p
