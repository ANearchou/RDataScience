

.get_root_url <- function(){ return("https://pro-api.coinmarketcap.com") }

.get_api_key <- function(){ return(Sys.getenv("CMC_API_KEY")) }


#' Retrieve all Cryptocurrencies listed on coinmarketcap
#'
#' @import data.table
#' @import curl
#' @import jsonlite
#' @family CMC API
#' @export
get_crypto_dim_map <- function(){
  
  root_url <- .get_root_url()
  crypto_map_url <- "/v1/cryptocurrency/map"
  api_key <- .get_api_key()
  
  h <- new_handle()
  handle_setheaders(h, "Accepts" = "application/json",
                    "X-CMC_PRO_API_KEY" = api_key)
  url <- paste0(root_url, crypto_map_url)
  res <- curl_fetch_memory(url, handle = h)
  
  if (res$status_code != 200L){
    stop(paste0("Error status code ", res$status_code, ": ", fromJSON(rawToChar(content), flatten = TRUE)$error))
  }
  
  content <- res$content
  dt <- fromJSON(rawToChar(content), flatten = TRUE)$data
  dt <- setDT(dt)[order(rank)]
  return(dt)
  
  
}




#' Retrieve top 100 Cryptocurrencies latest listing
#'
#' @import data.table
#' @import curl
#' @import jsonlite
#' @family CMC API
# ' @export
get_crypto_listings_latest <- function(){
  
  . <- cmc_rank <- id <-  NULL

  root_url <- .get_root_url()
  crypro_listings_latest_url <- "/v1/cryptocurrency/listings/latest"
  api_key <- .get_api_key()
  
  h <- new_handle()
  handle_setheaders(h, "Accepts" = "application/json",
                    "X-CMC_PRO_API_KEY" = api_key)
  url <- paste0(root_url, crypro_listings_latest_url)
  res <- curl_fetch_memory(url, handle = h)
  
  if (res$status_code != 200L){
    stop(paste0("Error status code ", res$status_code, ": ", fromJSON(rawToChar(content), flatten = TRUE)$error))
  }
  
  content <- res$content
  dt <- fromJSON(rawToChar(content), flatten = TRUE)$data
  dt <- setDT(dt)[order(cmc_rank)]
  
  tags <- dt[,.(id, tags)]
  tags[,tags := substr(tags, 3, nchar(tags))]
  tags[,tags := substr(tags, 1, nchar(tags)-1)]
  tags[,tags := gsub('"', "", tags)]
  tags <- tags[,.(tag = unlist(strsplit(tags, ","))), by = .(id)]
  
  return(list(
    listings_latest = dt,
    tags = tags
  ))
}



#' Retrieve the latest quotes from Cryptocurrencies
#'
#' @import data.table
#' @import curl
#' @import jsonlite
#' @param currency At what currency should the prices be converted
#' @param symbol Cryptocurrencies symbols
#' @param slug Cryptocurrencies slugs
#' @param id Cryptocurrencies id's
#' @family CMC API
# ' @export
get_crypto_quotes_latest <- function(currency = "USD",
                                     symbol = NULL,
                                     slug = NULL,
                                     id = NULL){
  
  tmp_id <- NULL
  
  if (is.null(symbol) & is.null(slug) & is.null(id)){
    stop("You must specify symbol, slug or id")
  }
  if (is.null(symbol) + is.null(slug) + is.null(id) != 2L){
    stop("Specify only one out of symbol, slug and id")
  }
  
  root_url <- .get_root_url()
  crypro_quotes_latest_url <- "/v1/cryptocurrency/quotes/latest?convert="
  api_key <- .get_api_key()
  
  h <- new_handle()
  handle_setheaders(h, "Accepts" = "application/json",
                    "X-CMC_PRO_API_KEY" = api_key)
  url <- paste0(root_url, crypro_quotes_latest_url, currency)
  
  if (!is.null(symbol)){
    url <- paste0(url, "&symbol=", paste(symbol, collapse = ","))
    len <- length(symbol)
  } else if (!is.null(slug)) {
    url <- paste0(url, "&slug=", paste(slug, collapse = ","))
    len <- length(slug)
  } else {
    url <- paste0(url, "&id=", paste(id, collapse = ","))
    len <- length(id)
  }
  res <- curl_fetch_memory(url, handle = h)
  
  if (res$status_code != 200L){
    stop(paste0("Error status code ", res$status_code, ": ", fromJSON(rawToChar(content), flatten = TRUE)$error))
  }
  
  content <- res$content
  dt <- fromJSON(rawToChar(content), flatten = TRUE)$data
  
  unlist_crypto <- function(x, c = currency){
    l <- length(unlist(names(x$quote[[c]])))
    t <- rbind(
      data.table(value = unlist(sapply(x$quote[[c]], function(y) ifelse(is.null(y), NA, y))),
                 statistic = unlist(names(x$quote[[c]])),
                 tmp_id = rep(unlist(x$symbol), l)),
      data.table(value = unlist(x$symbol), 
                 statistic = "symbol",
                 tmp_id = unlist(x$symbol)),
      data.table(value = c, 
                 statistic = "currency",
                 tmp_id = unlist(x$symbol))
    )
    return(t)
  }
  
  dt <- rbindlist(lapply(dt, unlist_crypto))
  dt <- dcast(dt, tmp_id~statistic, value.var = "value")
  dt[,tmp_id := NULL]
  
  return(dt)
}


