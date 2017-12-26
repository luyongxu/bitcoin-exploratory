#' ---
#' title: "Scrape Reddit"
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

#' # 2. Scrape Reddit 
reddit <- POST("http://redditmetrics.com/ajax/compare.reddits", body = list(reddit0 = "Bitcoin"), encode = "form")
reddit <- fromJSON(content(reddit, as = "text"))
reddit <- reddit[["message"]][["growth"]]
reddit <- reddit[["data"]] %>% 
  as_tibble() %>% 
  mutate(date = as.Date(y))


