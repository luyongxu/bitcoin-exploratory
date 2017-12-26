#' ---
#' title: "Scrape Google Trends Daily"
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

#' # 2. Scrape Google Trends Function 
google_daily <- function(keyword, begin_date, end_date, gprop) { 
  df <- gtrends(keyword = keyword, 
                time = str_c(begin_date, " ", end_date), 
                gprop = gprop)
  if (is.null(df[["interest_over_time"]])) { 
    return(tibble())
  } 
  else { 
  df <- df %>% 
    .[["interest_over_time"]] %>% 
    mutate(date = as.Date(date)) %>% 
    as_tibble()
  return(df)
  }
}

#' # 3. Set Download All Flag 
download_all <- FALSE
print(str_c("Google Trends daily download all flag is ", download_all, "."))

#' # 4. Create Date Set
if (download_all == TRUE) { 
  months <- c(ymd("2009-01-01") + months(0:ceiling(interval("2009-01-01", Sys.Date()) / months(1))))
}
if (download_all == FALSE) { 
  months <- c(ymd("2016-01-01") + months(0:ceiling(interval("2016-01-01", Sys.Date()) / months(1))))
}

#' # 5. Download Google Trends Daily Data 
for (i in seq_along(months)) { 
  if (months[i] + months(7) >= Sys.Date()) { 
    next
  } 
  begin_date <- months[i] 
  end_date <- months[i] + months(8)
  end_date <- as.Date(ifelse(end_date >= Sys.Date(), Sys.Date(), end_date))
  print(str_c("Downloading Google Trends daily data from ", begin_date, " to ", end_date, ".")) 
  
  # 5.1 Bitcoin Global Web 
  print("Downloading bitcoin.")
  df <- google_daily("bitcoin", begin_date, end_date, "web") 
  write_csv(df, str_c("./data/google-trends/bitcoin/bitcoin-", 
                      begin_date, "-", begin_date + months(8), ".csv"))
  Sys.sleep(5)
  
  # 5.2 Ethereum Global Web
  print("Downloading ethereum.")
  df <- google_daily("ethereum", begin_date, end_date, "web") 
  write_csv(df, str_c("./data/google-trends/ethereum/ethereum-", 
                      begin_date, "-", begin_date + months(8), ".csv"))
  Sys.sleep(5)
  
  # 5.3 Bitcoin Cash Global Web
  print("Downloading bitcoin cash.")
  df <- google_daily("bitcoin cash", begin_date, end_date, "web") 
  write_csv(df, str_c("./data/google-trends/bitcoin-cash/bitcoin-cash", 
                      begin_date, "-", begin_date + months(8), ".csv"))
  Sys.sleep(5)  
  
  # 5.4 Ripple Global Web 
  print("Downloading ripple.")
  df <- google_daily("ripple", begin_date, end_date, "web") 
  write_csv(df, str_c("./data/google-trends/ripple/ripple-", 
                      begin_date, "-", begin_date + months(8), ".csv"))
  Sys.sleep(5)    
  
  # 5.5 Dash Global Web 
  print("Downloading dash.")
  df <- google_daily("dash cryptocurrency", begin_date, end_date, "web") 
  write_csv(df, str_c("./data/google-trends/dash/dash-", 
                      begin_date, "-", begin_date + months(8), ".csv"))
  Sys.sleep(5)  
  
  # 5.6 Litecoin Global Web 
  print("Downloading litecoin.")
  df <- google_daily("litecoin", begin_date, end_date, "web") 
  write_csv(df, str_c("./data/google-trends/litecoin/litecoin-", 
                      begin_date, "-", begin_date + months(8), ".csv"))
  Sys.sleep(5)
}

#' # 6. Clean 
rm(months, i, begin_date, end_date, download_all, google_daily, df)

