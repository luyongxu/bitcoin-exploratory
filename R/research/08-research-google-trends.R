#' ---
#' title: "Research Google Trends"
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
source("./R/data/08-load-google-trends-daily.R")

#' # 3. Peak Detector 
peak_detector <- function(y, lag, threshold, influence) {
  signals <- rep(0, length(y))
  filteredY <- y[0:lag]
  avgFilter <- NULL
  stdFilter <- NULL
  avgFilter[lag] <- mean(y[0:lag])
  stdFilter[lag] <- sd(y[0:lag])
  for (i in (lag + 1):length(y)) { 
    if (is.na(y[i])) { 
      signals[i] <- 0
    }
    else if (abs(y[i] - avgFilter[i - 1]) > threshold * stdFilter[i - 1]) {
      if (y[i] > avgFilter[i - 1]) {
        signals[i] <- 1
      }
      else {
        signals[i] <- 0
      }
      filteredY[i] <- influence * y[i] + (1 - influence) * filteredY[i - 1]
    }
    else {
      signals[i] <- 0
      filteredY[i] <- y[i]
    }
    avgFilter[i] <- mean(filteredY[(i - lag):i], na.rm = TRUE)
    stdFilter[i] <- sd(filteredY[(i - lag):i], na.rm = TRUE)
  }
  return(list("signals" = signals,"avgFilter" = avgFilter, "stdFilter" = stdFilter))
}

#' # 4. Combined
combined <- bitcoin_price %>% 
  left_join(google_trends %>% 
              filter(keyword == "bitcoin") %>% 
              mutate(date = as.Date(date))) %>% 
  select(date, close, hits_rebased)
combined2 <- combined %>% 
  filter(date >= "2011-01-01", 
         date <= "2018-01-01", 
         !is.na(hits_rebased)) %>% 
  mutate(hits_rebased_plot = hits_rebased / (mean(hits_rebased, na.rm = TRUE) / mean(close, na.rm = TRUE)), 
         peaks = peak_detector(hits_rebased_plot, lag = 7, threshold = 3, influence = 0.1)[["signals"]], 
         avg = peak_detector(hits_rebased_plot, lag = 7, threshold = 3, influence = 0.1)[["avgFilter"]], 
         std = peak_detector(hits_rebased_plot, lag = 7, threshold = 3, influence = 0.1)[["stdFilter"]], 
         after_peak = ifelse(roll_meanr(peaks, 10, fill = 0) > 0, 1, 0), 
         future_return = lead(close, 1) / close - 1, 
         future_return_sign = ifelse(future_return > 0, 1, 0))
ggplot(combined2, aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue") + 
  geom_line(aes(y = hits_rebased_plot), colour = "red") + 
  geom_line(aes(y = avg), colour = "green") + 
  geom_line(aes(y = avg + 3 * std), colour = "green") + 
  geom_point(aes(y = close), colour = "blue") + 
  geom_point(aes(y = hits_rebased_plot), colour = "red")
ggplot(combined2, aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue") + 
  geom_line(aes(y = hits_rebased_plot , colour = peaks)) + 
  geom_point(aes(y = close), colour = "blue") + 
  geom_point(aes(y = hits_rebased_plot, colour = peaks)) + 
  scale_colour_gradient2(low = "red", mid = "black", high = "green") 
ggplot(combined2, aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue") + 
  geom_line(aes(y = hits_rebased_plot , colour = after_peak)) + 
  geom_point(aes(y = close), colour = "blue") + 
  geom_point(aes(y = hits_rebased_plot, colour = after_peak)) + 
  scale_colour_gradient2(low = "red", mid = "black", high = "green") 


ggplot(combined2, aes(x = future_return, fill = factor(peaks))) + 
  geom_histogram(position = "identity", alpha = 0.25, binwidth = 0.005) + 
  coord_cartesian(xlim = c(-0.2, 0.2))
ggplot(combined2, aes(x = future_return, fill = factor(peaks))) + 
  geom_density(alpha = 0.25) + 
  coord_cartesian(xlim = c(-0.2, 0.2))
ggplot(combined2, aes(x = factor(peaks), y = future_return, fill = factor(peaks))) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(-0.1, 0.1))
table(combined2$peaks, combined2$future_return_sign)
prop.table(table(combined2$peaks, combined2$future_return_sign), margin = 1)

ggplot(combined2, aes(x = future_return, fill = factor(after_peak))) + 
  geom_histogram(position = "identity", alpha = 0.25, binwidth = 0.005) + 
  coord_cartesian(xlim = c(-0.2, 0.2))
ggplot(combined2, aes(x = future_return, fill = factor(after_peak))) + 
  geom_density(alpha = 0.25) + 
  coord_cartesian(xlim = c(-0.2, 0.2))
ggplot(combined2, aes(x = factor(after_peak), y = future_return, fill = factor(after_peak))) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(-0.1, 0.1))
table(combined2$after_peak, combined2$future_return_sign)
prop.table(table(combined2$after_peak, combined2$future_return_sign), margin = 1)

#' # 5. 
#' August 1, 2017: Bitcoin split in two 
#' August 13, 2017: Bitcoin surges past $4000, sets more records
#' August 14, 2017: Bitcoin Streaks To $4300 Mark, Continuing Meteoric Rise
#' August 15, 2017: Bitcoin hits a record high before rolling over
#' September 13, 2017: Bitcoin is a fraud that will blow up, says JP Morgan boss
#' September 14, 2017: Bitcoin is tumbling after Chinese regulators say an exchange ban is certain
#' September 15. 2017: China's bitcoin crackdown forces exchanges to close. The price is crashing
