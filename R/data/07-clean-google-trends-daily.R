#' ---
#' title: "Clean Google Trends Daily"
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

#' # 2. Load Data Function
load_data <- function(path, col_types) { 
  df_combined <- tibble()
  for (file in list.files(path)) { 
    df <- read_csv(str_c(path, file), col_types = col_types)
    if (nrow(df) == 0) { 
      next
    }
    else { 
      df <- df %>% mutate(hits_change_01d = hits / lag(hits, 1) - 1)
      df_combined <- bind_rows(df_combined, df)
    }
  }
  print(str_c("Loaded files in ", path, "."))
  return(df_combined)
}

#' # 3. Load Google Trends Daily Data 
#' Filter data to start when interest over time ceases to contain zero search volume. 
google_bitcoin <- load_data("./data/google-trends/bitcoin/", col_types = "Ticccid") %>% 
  filter(date > "2011-02-09")
google_ethereum <- load_data("./data/google-trends/ethereum/", col_types = "Ticccid") %>% 
  filter(date > "2014-01-10")
google_bitcoin_cash <- load_data("./data/google-trends/bitcoin-cash/", col_types = "Ticccid") %>% 
  filter(date > "2017-07-15")
google_ripple <- load_data("./data/google-trends/ripple/", col_types = "Ticccid") %>% 
  filter(date > "2009-01-02")
google_dash <- load_data("./data/google-trends/dash/", col_types = "Ticccid") %>% 
  filter(date > "2009-01-02")
google_litecoin <- load_data("./data/google-trends/litecoin/", col_types = "Ticccid") %>% 
  filter(date > "2013-01-18")

#' # 4. Combine Data 
google <- bind_rows(google_bitcoin, google_ethereum, google_bitcoin_cash, google_ripple, google_dash, google_litecoin)

#' # 5. Calculate Average Hits Daily Change
google <- google %>% 
  group_by(keyword, date) %>% 
  summarise(hits_mean = mean(hits, na.rm = TRUE), 
            hits_change_01d = mean(hits_change_01d, na.rm = TRUE)) 

#' # 6. Calculate Hits Rebased 
google <- google %>%  
  group_by(keyword) %>% 
  filter(is.numeric(hits_change_01d)) %>% 
  mutate(hits_rebased = cumprod(1 + hits_change_01d))

#' # 7. Save Data
write_csv(google, "./Raw Data/google-trends/google-trends-clean.csv")

#' # 8. Clean
rm(google, google_bitcoin, google_bitcoin_cash, google_dash, google_ethereum, google_litecoin, google_ripple, load_data)
