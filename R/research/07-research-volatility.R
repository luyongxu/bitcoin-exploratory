#' ---
#' title: "Research Volatility"
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

#' # 3. Calculate Volatility 
volatility <- coinmarketcap %>% 
  group_by(id) %>% 
  mutate(close_return_01d = close / lag(close, 1) - 1, 
         close_sd_10 = roll_sdr(close_return_01d, n = 10), 
         close_sd_20 = roll_sdr(close_return_01d, n = 20), 
         close_sd_30 = roll_sdr(close_return_01d, n = 30), 
         close_sd_40 = roll_sdr(close_return_01d, n = 40), 
         close_sd_50 = roll_sdr(close_return_01d, n = 50), 
         close_sd_60 = roll_sdr(close_return_01d, n = 60), 
         close_sd_70 = roll_sdr(close_return_01d, n = 70), 
         close_sd_80 = roll_sdr(close_return_01d, n = 80), 
         close_sd_90 = roll_sdr(close_return_01d, n = 90), 
         close_sd_120 = roll_sdr(close_return_01d, n = 120), 
         close_sd_150 = roll_sdr(close_return_01d, n = 150), 
         close_sd_180 = roll_sdr(close_return_01d, n = 180), 
         close_sd_210 = roll_sdr(close_return_01d, n = 210), 
         close_sd_360 = roll_sdr(close_return_01d, n = 360), 
         close_sd_720 = roll_sdr(close_return_01d, n = 720)) %>% 
  ungroup() %>% 
  mutate(name_sorted = reorder(name, rank)) 

#' # 4. Calculate Average Volatility 
volatility <- volatility %>% 
  left_join(volatility %>% 
              filter(rank <= 12) %>% 
              group_by(date) %>% 
              summarise(close_sd_30_avg = mean(close_sd_30, na.rm = TRUE))) %>% 
  left_join(volatility %>% 
              filter(rank <= 12) %>% 
              group_by(date) %>% 
              summarise(close_sd_90_avg = mean(close_sd_90, na.rm = TRUE))) %>% 
  left_join(volatility %>% 
              filter(rank <= 12) %>% 
              group_by(date) %>% 
              summarise(close_sd_180_avg = mean(close_sd_180, na.rm = TRUE))) %>% 
  left_join(volatility %>% 
              filter(rank <= 12) %>% 
              group_by(date) %>% 
              summarise(close_sd_360_avg = mean(close_sd_360, na.rm = TRUE))) 

#' # 5. Plot 
ggplot(volatility %>% filter(rank <= 12), aes(x = date)) + 
  geom_line(aes(y = close_sd_30, colour = name)) + 
  facet_wrap(~ name_sorted, ncol = 4, scales = "free_y")
ggplot(volatility %>% filter(rank <= 12), aes(x = date)) + 
  geom_line(aes(y = close_sd_30, colour = name_sorted))
ggplot(volatility %>% filter(rank <= 12), aes(x = date)) + 
  geom_line(aes(y = close_sd_30_avg), colour = "red")
ggplot(volatility %>% filter(id == "bitcoin"), aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue") + 
  geom_line(aes(y = close_sd_30_avg * mean(close, na.rm = TRUE) / mean(close_sd_30_avg, na.rm = TRUE)), colour = "red") + 
  scale_y_continuous(trans = "log2")
ggplot(volatility %>% filter(id == "bitcoin"), aes(x = lag(close_sd_30_avg, 1), y = close_return_01d)) + 
  geom_point(colour = "blue", alpha = 0.25) + 
  geom_hline(yintercept = 0)
ggplot(volatility %>% filter(id == "bitcoin"), aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue") + 
  geom_line(aes(y = close_sd_30 * mean(close, na.rm = TRUE) / mean(close_sd_30, na.rm = TRUE)), colour = "red") + 
  scale_y_continuous(trans = "log2")
ggplot(volatility %>% filter(id == "bitcoin"), aes(x = lag(close_sd_30, 1), y = close_return_01d)) + 
  geom_point(colour = "blue", alpha = 0.25) + 
  geom_hline(yintercept = 0)


ggplot(volatility %>% filter(rank <= 12), aes(x = date)) + 
  geom_line(aes(y = close_sd_90, colour = name)) + 
  facet_wrap(~ name_sorted, ncol = 4, scales = "free_y")
ggplot(volatility %>% filter(rank <= 12), aes(x = date)) + 
  geom_line(aes(y = close_sd_90, colour = name_sorted))
ggplot(volatility %>% filter(rank <= 12), aes(x = date)) + 
  geom_line(aes(y = close_sd_90_avg), colour = "red")
ggplot(volatility %>% filter(id == "bitcoin"), aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue") + 
  geom_line(aes(y = close_sd_90_avg * mean(close, na.rm = TRUE) / mean(close_sd_90_avg, na.rm = TRUE)), colour = "red") + 
  scale_y_continuous(trans = "log2")
ggplot(volatility %>% filter(id == "bitcoin"), aes(x = lag(close_sd_90_avg, 1), y = close_return_01d)) + 
  geom_point(colour = "blue", alpha = 0.25) + 
  geom_hline(yintercept = 0)
ggplot(volatility %>% filter(id == "bitcoin"), aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue") + 
  geom_line(aes(y = close_sd_30 * mean(close, na.rm = TRUE) / mean(close_sd_90, na.rm = TRUE)), colour = "red") + 
  scale_y_continuous(trans = "log2")
ggplot(volatility %>% filter(id == "bitcoin"), aes(x = lag(close_sd_90, 1), y = close_return_01d)) + 
  geom_point(colour = "blue", alpha = 0.25) + 
  geom_hline(yintercept = 0)

ggplot(volatility %>% filter(rank <= 12), aes(x = date)) + 
  geom_line(aes(y = close_sd_180, colour = name)) + 
  facet_wrap(~ name_sorted, ncol = 4, scales = "free_y")
ggplot(volatility %>% filter(rank <= 12), aes(x = date)) + 
  geom_line(aes(y = close_sd_180, colour = name_sorted))
ggplot(volatility %>% filter(rank <= 12), aes(x = date)) + 
  geom_line(aes(y = close_sd_180_avg), colour = "red")
ggplot(volatility %>% filter(id == "bitcoin"), aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue") + 
  geom_line(aes(y = close_sd_180_avg * mean(close, na.rm = TRUE) / mean(close_sd_180_avg, na.rm = TRUE)), colour = "red") + 
  scale_y_continuous(trans = "log2")
ggplot(volatility %>% filter(id == "bitcoin"), aes(x = lag(close_sd_180_avg, 1), y = close_return_01d)) + 
  geom_point(colour = "blue", alpha = 0.25) + 
  geom_hline(yintercept = 0)
ggplot(volatility %>% filter(id == "bitcoin"), aes(x = date)) + 
  geom_line(aes(y = close), colour = "blue") + 
  geom_line(aes(y = close_sd_30 * mean(close, na.rm = TRUE) / mean(close_sd_180, na.rm = TRUE)), colour = "red") + 
  scale_y_continuous(trans = "log2")
ggplot(volatility %>% filter(id == "bitcoin"), aes(x = lag(close_sd_180, 1), y = close_return_01d)) + 
  geom_point(colour = "blue", alpha = 0.25) + 
  geom_hline(yintercept = 0)

