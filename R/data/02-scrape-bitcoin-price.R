#' ---
#' title: "Scrape Bitcoin Price"
#' author: "Kevin Lu"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output: 
#'   html_document: 
#'     theme: default 
#'     highlight: tango
#'     toc: true 
#'     toc_float: true
#'     number_sections: false
#'     fig_width: 10
#'     fig_height: 5 
#' ---

#' # 1. Load Data Packages and Functions
source("./R/data/01-load-packages.R")

#' # 2. Quandl Function
#' The quandl_tidy function is a wrapper around the Quandl function that returns a cleaner tibble.  
Quandl.api_key("3GAtxPrAgoah7PyADPGy")
quandl_tidy <- function(code, name) { 
  df <- Quandl(code) %>% 
    mutate(code = code, name = name) %>% 
    rename(date = Date, value = Value) %>% 
    arrange(date) %>% 
    as_tibble()
  return(df)
}

#' # 3. Bitcoin Exchange Rate Data
bitcoin_price <- Quandl("BCHARTS/BITSTAMPUSD") %>% 
  arrange(Date) %>% 
  as_tibble()
colnames(bitcoin_price) <- c("date", "open", "high", "low", "close", "volume_btc", "volume_currency", "weighted_price")

#' # 4. Clean Bitcoin Price
#' Data errors are cleaned by using last observation carried forward. 
bitcoin_price[bitcoin_price == 0] <- NA
bitcoin_price <- bitcoin_price %>% 
  map_df(na.locf)

#' # 5. Print and Plot
print(bitcoin_price)
ggplot(bitcoin_price, aes(x = date, y = close)) + 
  geom_line(colour = "blue")

#' # 6. Clean
rm(quandl_tidy)
