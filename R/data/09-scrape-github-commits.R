#' ---
#' title: "Scrape Github Commits"
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

#' # 2. Get Github Contributor Statistics
github <- fromJSON("https://api.github.com/repos/bitcoin/bitcoin/stats/contributors", flatten = TRUE)

#' # 3. Clean Data 
weeks <- github[["weeks"]] %>% 
  map_df(bind_rows) %>% 
  as_tibble() %>% 
  group_by(w) %>% 
  summarise(a = sum(a), 
            d = sum(d), 
            c = sum(c)) %>% 
  mutate(date = as.Date(as.POSIXct(w, origin = "1970-01-01")))
