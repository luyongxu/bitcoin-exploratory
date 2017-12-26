#' ---
#' title: "Research Prices"
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

#' # 1. Load Research Packages and Functions 
source("./R/research/01-load-packages.R")

#' # 2. Load Data 
source("./R/data/05-load-coinmarketcap.R")

#' # 3. Prepare Data for Plotting
volume_rank <- coinmarketcap %>% 
  filter(date >= max(date) - days(30)) %>% 
  group_by(id) %>% 
  summarise(volume_total = sum(volume)) %>% 
  arrange(desc(volume_total)) %>% 
  mutate(volume_rank = row_number())
coinmarketcap <- coinmarketcap %>% 
  left_join(volume_rank) %>% 
  rename(marketcap_rank = rank) %>% 
  mutate(name_plot = str_c(name, " (MC", marketcap_rank, ", V", volume_rank, ")"), 
         name_plot = reorder(name_plot, marketcap_rank)) %>% 
  group_by(id) %>% 
  mutate(close_latest = ifelse(date == max(date), close, NA)) %>% 
  filter(marketcap_rank <= 200)

#' # 4. Create Table
coinmarketcap_table <- coinmarketcap %>% 
  select(date, name, id, close, market_cap, volume_total, marketcap_rank, volume_rank) %>% 
  mutate(market_cap = round(market_cap / 1000000, 2), 
         volume_total = round(volume_total / 1000000, 2), 
         close_07d_return = round(close / lag(close, 7) - 1, 2), 
         close_01m_return = round(close / lag(close, 30) - 1, 2), 
         close_06m_return = round(close / lag(close, 180) - 1, 2), 
         close_12m_return = round(close / lag(close, 365) - 1, 2)) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  select(-date, -id) %>% 
  select(`Name` = name, 
         `Price` = close, 
         `Market Cap` = market_cap, 
         `Volume (30d)` = volume_total, 
         `Market Cap Rank` = marketcap_rank, 
         `Volume Rank` = volume_rank, 
         `Close (7d Chg)` = close_07d_return, 
         `Close (1m Chg)` = close_01m_return, 
         `Close (6m Chg)` = close_06m_return, 
         `Close (12m Chg)` = close_12m_return)

#' # 5. Plot Data
plot_list_a <- list()
for (i in seq(from = 8, to = 200, by = 8)) {
  local({
    i <- i
    plot_list_a[[i / 8]] <<- ggplot(coinmarketcap %>%
                                      filter(marketcap_rank > i - 8, marketcap_rank <= i,
                                             date >= Sys.Date() - months(3)),
                                    aes(x = date, y = close)) +
      geom_line(aes(y = close), colour = "blue") +
      geom_point(aes(y = close_latest), colour = "red") +
      facet_wrap(~ name_plot, ncol = 4, scales = "free_y")  +
      scale_y_continuous(trans = "log2", labels = function(x) round(x, 2)) +
      labs(x = "Date", y = "Price in USD") +
      theme(axis.title.x = element_blank())
  })
}
plot_list_b <- list()
for (i in seq(from = 8, to = 200, by = 8)) {
  local({
    i <- i
    plot_list_b[[i / 8]] <<- ggplot(coinmarketcap %>%
                                      filter(marketcap_rank > i - 8, marketcap_rank <= i,
                                             date >= Sys.Date() - months(6)),
                                    aes(x = date, y = close)) +
      geom_line(aes(y = close), colour = "blue") +
      geom_point(aes(y = close_latest), colour = "red") +
      facet_wrap(~ name_plot, ncol = 4, scales = "free_y")  +
      scale_y_continuous(trans = "log2", labels = function(x) round(x, 2)) +
      labs(x = "Date", y = "Price in USD") +
      theme(axis.title.x = element_blank())
  })
}
plot_list_c <- list()
for (i in seq(from = 8, to = 200, by = 8)) {
  local({
    i <- i
    plot_list_c[[i / 8]] <<- ggplot(coinmarketcap %>%
                                      filter(marketcap_rank > i - 8, marketcap_rank <= i,
                                             date >= Sys.Date() - years(1)),
                                    aes(x = date, y = close)) +
      geom_line(aes(y = close), colour = "blue") +
      geom_point(aes(y = close_latest), colour = "red") +
      facet_wrap(~ name_plot, ncol = 4, scales = "free_y")  +
      scale_y_continuous(trans = "log2", labels = function(x) round(x, 2)) +
      labs(x = "Date", y = "Price in USD") +
      theme(axis.title.x = element_blank())
  })
}
plot_list_d <- list()
for (i in seq(from = 8, to = 200, by = 8)) {
  local({
    i <- i
    plot_list_d[[i / 8]] <<- ggplot(coinmarketcap %>%
                                      filter(marketcap_rank > i - 8, marketcap_rank <= i,
                                             date >= Sys.Date() - years(2)),
                                    aes(x = date, y = close)) +
      geom_line(aes(y = close), colour = "blue") +
      geom_point(aes(y = close_latest), colour = "red") +
      facet_wrap(~ name_plot, ncol = 4, scales = "free_y")  +
      scale_y_continuous(trans = "log2", labels = function(x) round(x, 2)) +
      labs(x = "Date", y = "Price in USD") +
      theme(axis.title.x = element_blank())
  })
}
plot_list_e <- list()
for (i in seq(from = 8, to = 200, by = 8)) {
  local({
    i <- i
    plot_list_e[[i / 8]] <<- ggplot(coinmarketcap %>%
                                      filter(marketcap_rank > i - 8, marketcap_rank <= i,
                                             date >= Sys.Date() - years(4)),
                                    aes(x = date, y = close)) +
      geom_line(aes(y = close), colour = "blue") +
      geom_point(aes(y = close_latest), colour = "red") +
      facet_wrap(~ name_plot, ncol = 4, scales = "free_y")  +
      scale_y_continuous(trans = "log2", labels = function(x) round(x, 2)) +
      labs(x = "Date", y = "Price in USD") +
      theme(axis.title.x = element_blank())
  })
}