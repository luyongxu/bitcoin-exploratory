#' ---
#' title: "Research Wikipedia"
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
source("./R/data/02-scrape-bitcoin-price.R")
source("./R/data/03-scrape-wikipedia-views.R")

#' # 3. Combine 
combined <- bitcoin_wiki %>% 
  left_join(bitcoin_price) %>% 
  select(project, date, views, close) %>% 
  group_by(project) %>% 
  mutate(views_change_07d = views / lag(views, 7) - 1, 
         views_peak_985 = ifelse(views_change_07d >= quantile(views_change_07d, probs = 0.985, na.rm = TRUE), close, NA), 
         views_peak_990 = ifelse(views_change_07d >= quantile(views_change_07d, probs = 0.990, na.rm = TRUE), close, NA)) %>% 
  ungroup()

#' # 4. Plot 
plot_list_a <- list()
for (i in unique(combined[["project"]])) { 
  local({ 
    i <- i
    df <- combined %>% filter(project == i)
    scale_factor <- mean(df[["close"]] / df[["views"]])
    plot_list_a[[i]] <<- ggplot(df, aes(x = date)) + 
      geom_line(aes(y = close), colour = "blue", size = 0.8) + 
      geom_line(aes(y = views * scale_factor), colour = "red", alpha = 0.8) + 
      scale_y_continuous(trans = "log2", sec.axis = sec_axis(~ . * 1 / scale_factor, name = "Wikipedia Page Views")) + 
      labs(title = str_c("Bitcoin Price vs ", i, " Page Views"), y = "Bitcoin Price", x = "Date")
  })
}

plot_list_b <- list()
for (i in unique(combined[["project"]])) { 
  local({
    i <- i
    df <- combined %>% 
      filter(project == i) %>% 
      select(date, close, views_change_07d) %>% 
      gather(key, value, -date)
    plot_list_b[[i]] <<- ggplot(df, aes(x = date, color = key)) + 
      geom_line(aes(y = value), size = 0.8) + 
      scale_y_continuous(trans = "log1p") + 
      scale_color_manual(values = c("blue", "red")) + 
      facet_wrap(~ key, ncol = 1, scales = "free", 
                 labeller = as_labeller(c("close" = "Bitcoin Price", "views_change_07d" = "Wikipedia Page Views (7d Change)"))) + 
      labs(title = str_c("Bitcoin Price vs ", i, " Page Views (7d Chg)"), y = "", x = "Date") + 
      theme(legend.position = "none")
  })
}

plot_list_c <- list()
for (i in unique(combined[["project"]])) { 
  local({ 
    i <- i 
    df <- combined %>% filter(project == i)
    plot_list_c[[i]] <<- ggplot(df, aes(x = date)) + 
      geom_point(aes(y = close, colour = views_change_07d, size = views_change_07d)) + 
      scale_y_continuous(trans = "log2") + 
      scale_colour_gradient2(low = "light blue", high = "red", mid = "light blue", midpoint = 0) + 
      scale_size(range = c(1, 6)) + 
      labs(title = str_c("Bitcoin Price With ", i, " Page Views Signal"), y = "Bitcoin Price", x = "Date", 
           colour = "Views (7-Day Chg)", size = "Views (7-Day Chg)")
  })
}

plot_list_d <- list() 
for (i in unique(combined[["project"]])) { 
  local({ 
    i <- i 
    df <- combined %>% filter(project == i) 
    plot_list_d[[i]] <<- ggplot(df, aes(x = date)) + 
      geom_line(aes(y = close), colour = "blue", size = 0.8) + 
      geom_point(aes(y = views_peak_985), colour = "red", size = 3) + 
      geom_point(aes(y = views_peak_990), colour = "green", size = 3) + 
      scale_y_continuous(trans = "log2") + 
      labs(title = str_c("Bitcoin Price With ", i, " Page Views Threshold"), y = "Bitcoin Price", x = "Date")
  })
}

#' # 5. Clean 
rm(i)
