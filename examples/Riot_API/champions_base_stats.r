
library(data.table)
library(curl)
library(jsonlite)

h <- new_handle()
handle_setheaders(h, "Accepts" = "application/json",
                  "LOL_API_KEY" = apikey)

champions <- curl_fetch_memory(
  "http://ddragon.leagueoflegends.com/cdn/11.23.1/data/en_US/champion.json",
  handle = h
)
champions <- fromJSON(rawToChar(champions$content), flatten = T)
champions <- champions$data

champ_stats <- c()
for (i in 1:length(champions)){
  champ_stats <- unique(c(champ_stats, names(champions[[i]]$stats)))
}

champions_base_stats <- data.table()
for (i in 1:length(champions)){
  len <- length(champions[[i]]$stats)
  champions_base_stats <- rbind(
    champions_base_stats,
    data.table(
      champion = rep(names(champions[i]), len),
      statistic = names(champions[[i]]$stats),
      value = unlist(champions[[i]]$stats)
    )
  )
}


champions_base_stats[,value_std := (value - min(value)) / (max(value) - min(value)), by = .(statistic)]
champions_base_stats[is.na(value_std), value_std := 0] 
