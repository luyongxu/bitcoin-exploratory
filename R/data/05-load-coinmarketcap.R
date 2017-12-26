#' ---
#' title: "Load CoinMarketCap"
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

#' # 2. Load CoinMarketCap 
coinmarketcap <- read_csv("./data/coinmarketcap/coinmarketcap-prices.csv", 
                          col_types = "Dddddddccccd")
