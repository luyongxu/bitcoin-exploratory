#' ---
#' title: "Scrape CoinMarketCap"
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

#' # 2. Download URL Suffix of All Coins
url_root <- read_html("https://coinmarketcap.com/all/views/all/") %>% 
  html_nodes(".currency-name a") %>% 
  html_attr("href") %>% 
  unique()
url_table <- read_html("https://coinmarketcap.com/all/views/all/") %>% 
  html_table() %>% 
  .[[1]] %>% 
  as_tibble() %>% 
  mutate(url_root = url_root, 
         type = str_match(url_root, "/(.*)/(.*)/")[, 2], 
         id = str_match(url_root, "/(.*)/(.*)/")[, 3]) %>% 
  select(id, type, url_root)

#' # 3. Query CoinMarketCap Ticker
ticker <- fromJSON("https://api.coinmarketcap.com/v1/ticker/") %>% 
  as_tibble() %>% 
  left_join(url_table)

#' # 4. Scrape Historical Data 
coinmarketcap <- tibble()
for (i in 1:nrow(ticker)) { 
  print(str_c(i, ": Downloading data for ", ticker[[i, "name"]], "."))
  url <- str_c("https://coinmarketcap.com", 
               ticker[[i, "url_root"]], 
               "historical-data/?start=19000101&end=", 
               format(Sys.Date(), "%Y%m"))
  try({
    df <- read_html(url) %>% 
      html_table() %>% 
      .[[1]] %>% 
      as_tibble() %>% 
      mutate(id = ticker[[i, "id"]], 
             name = ticker[[i, "name"]], 
             type = ticker[[i, "type"]], 
             symbol = ticker[[i, "symbol"]], 
             rank = ticker[[i, "rank"]]) %>% 
      mutate_all(funs(as.character(.)))
    coinmarketcap <- coinmarketcap %>% 
      bind_rows(df)
  })
  Sys.sleep(2.5)
}

#' # 5. Clean Historical Data
colnames(coinmarketcap) <- c("date", "open", "high", "low", "close", "volume", 
                             "market_cap", "id", "name", "type", "symbol", "rank")
coinmarketcap <- coinmarketcap %>% 
  filter(date != "No data was found for the selected time period.") %>% 
  mutate_all(funs(ifelse(. == "-", NA, .))) %>% 
  mutate(date = as.Date(mdy(date)), 
         open = as.numeric(open), 
         high = as.numeric(high), 
         low = as.numeric(low), 
         close = as.numeric(close), 
         volume = str_replace_all(volume, ",", ""), 
         volume = as.numeric(volume), 
         market_cap = str_replace_all(market_cap, ",", ""), 
         market_cap = as.numeric(market_cap), 
         rank = as.numeric(rank)) %>% 
  arrange(rank, id, date) 

#' # 6. Save Data
write_csv(coinmarketcap, "./data/coinmarketcap/coinmarketcap-prices.csv") 

#' # 7. Clean
rm(df, url, i, url_root, url_table, ticker)
