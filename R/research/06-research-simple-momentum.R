#' ---
#' title: "Research Simple Momentum"
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

#' # 3. Simple Momentum
momentum <- coinmarketcap %>% 
  group_by(id) %>% 
  mutate(close_return_01d = close / lag(close, 1) - 1, 
         close_return_01d = ifelse(is.na(close_return_01d), 0, close_return_01d), 
         close_return_05d = exp(log(close) - roll_meanr(log(close), n = 05)) - 1, 
         close_return_10d = exp(log(close) - roll_meanr(log(close), n = 10)) - 1, 
         close_return_15d = exp(log(close) - roll_meanr(log(close), n = 15)) - 1, 
         close_return_20d = exp(log(close) - roll_meanr(log(close), n = 20)) - 1, 
         close_return_25d = exp(log(close) - roll_meanr(log(close), n = 25)) - 1, 
         close_return_30d = exp(log(close) - roll_meanr(log(close), n = 30)) - 1, 
         close_return_35d = exp(log(close) - roll_meanr(log(close), n = 35)) - 1, 
         close_return_40d = exp(log(close) - roll_meanr(log(close), n = 40)) - 1, 
         close_return_45d = exp(log(close) - roll_meanr(log(close), n = 45)) - 1, 
         close_return_50d = exp(log(close) - roll_meanr(log(close), n = 50)) - 1, 
         close_return_55d = exp(log(close) - roll_meanr(log(close), n = 55)) - 1, 
         close_return_60d = exp(log(close) - roll_meanr(log(close), n = 60)) - 1, 
         signal_05d = ifelse(lag(close_return_05d) > 0.00, 1, 0), 
         signal_10d = ifelse(lag(close_return_10d) > 0.00, 1, 0), 
         signal_15d = ifelse(lag(close_return_15d) > 0.00, 1, 0), 
         signal_20d = ifelse(lag(close_return_20d) > 0.00, 1, 0), 
         signal_25d = ifelse(lag(close_return_25d) > 0.00, 1, 0), 
         signal_30d = ifelse(lag(close_return_30d) > 0.00, 1, 0), 
         signal_35d = ifelse(lag(close_return_35d) > 0.00, 1, 0), 
         signal_40d = ifelse(lag(close_return_40d) > 0.00, 1, 0), 
         signal_45d = ifelse(lag(close_return_45d) > 0.00, 1, 0), 
         signal_50d = ifelse(lag(close_return_50d) > 0.00, 1, 0), 
         signal_55d = ifelse(lag(close_return_55d) > 0.00, 1, 0), 
         signal_60d = ifelse(lag(close_return_60d) > 0.00, 1, 0))

#' # 4. Create Momentum Ensemble
momentum <- momentum %>% 
  rowwise() %>% 
  mutate(signal_avg = mean(c(signal_05d, signal_10d, signal_15d, signal_20d, signal_25d, signal_30d, 
                             signal_35d, signal_40d, signal_45d, signal_50d, signal_55d, signal_60d), 
                           na.rm = TRUE))

#' # 5. Calculate Model Returns
momentum <- momentum %>% 
  group_by(id) %>% 
  mutate_at(vars(signal_05d:signal_avg), funs(ifelse(is.na(.), 1, .))) %>% 
  mutate(return_buyhold = cumprod(1 + close_return_01d) - 1, 
         return_momentum_05d = cumprod(1 + close_return_01d * signal_05d) - 1, 
         return_momentum_10d = cumprod(1 + close_return_01d * signal_10d) - 1, 
         return_momentum_15d = cumprod(1 + close_return_01d * signal_15d) - 1, 
         return_momentum_20d = cumprod(1 + close_return_01d * signal_20d) - 1, 
         return_momentum_25d = cumprod(1 + close_return_01d * signal_25d) - 1, 
         return_momentum_30d = cumprod(1 + close_return_01d * signal_30d) - 1, 
         return_momentum_35d = cumprod(1 + close_return_01d * signal_35d) - 1, 
         return_momentum_40d = cumprod(1 + close_return_01d * signal_40d) - 1, 
         return_momentum_45d = cumprod(1 + close_return_01d * signal_45d) - 1, 
         return_momentum_50d = cumprod(1 + close_return_01d * signal_50d) - 1, 
         return_momentum_55d = cumprod(1 + close_return_01d * signal_55d) - 1, 
         return_momentum_60d = cumprod(1 + close_return_01d * signal_60d) - 1, 
         return_momentum_avg = cumprod(1 + close_return_01d * signal_avg) - 1) %>% 
  ungroup() %>% 
  mutate(name_sorted = reorder(name, rank))

#' # 6. Plot
ggplot(momentum %>% filter(rank <= 12), aes(x = date)) + 
  geom_line(aes(y = return_buyhold + 1), colour = "blue", size = 1.5) + 
  geom_line(aes(y = return_momentum_05d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_10d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_15d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_20d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_25d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_30d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_35d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_40d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_45d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_50d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_55d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_60d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_avg + 1), colour = "red", alpha = 0.8) + 
  scale_y_continuous(trans = "log2") + 
  facet_wrap(~ name_sorted, ncol = 4, scales = "free_y") 

ggplot(momentum %>% filter(rank > 12, rank <= 24), aes(x = date)) + 
  geom_line(aes(y = return_buyhold + 1), colour = "blue", size = 1.5) + 
  geom_line(aes(y = return_momentum_05d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_10d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_15d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_20d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_25d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_30d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_35d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_40d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_45d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_50d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_55d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_60d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_avg + 1), colour = "red", alpha = 0.8) + 
  scale_y_continuous(trans = "log2") +
  facet_wrap(~ name_sorted, ncol = 4, scales = "free_y") 

ggplot(momentum %>% filter(rank > 24, rank <= 36), aes(x = date)) + 
  geom_line(aes(y = return_buyhold + 1), colour = "blue", size = 1.5) + 
  geom_line(aes(y = return_momentum_05d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_10d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_15d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_20d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_25d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_30d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_35d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_40d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_45d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_50d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_55d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_60d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_avg + 1), colour = "red", alpha = 0.8) + 
  scale_y_continuous(trans = "log2") +
  facet_wrap(~ name_sorted, ncol = 4, scales = "free_y") 

ggplot(momentum %>% filter(rank > 36, rank <= 48), aes(x = date)) + 
  geom_line(aes(y = return_buyhold + 1), colour = "blue", size = 1.5) + 
  geom_line(aes(y = return_momentum_05d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_10d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_15d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_20d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_25d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_30d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_35d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_40d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_45d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_50d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_55d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_60d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_avg + 1), colour = "red", alpha = 0.8) + 
  scale_y_continuous(trans = "log2") +
  facet_wrap(~ name_sorted, ncol = 4, scales = "free_y") 

ggplot(momentum %>% filter(rank > 48, rank <= 60), aes(x = date)) + 
  geom_line(aes(y = return_buyhold + 1), colour = "blue", size = 1.5) + 
  geom_line(aes(y = return_momentum_05d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_10d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_15d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_20d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_25d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_30d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_35d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_40d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_45d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_50d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_55d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_60d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_avg + 1), colour = "red", alpha = 0.8) + 
  scale_y_continuous(trans = "log2") +
  facet_wrap(~ name_sorted, ncol = 4, scales = "free_y") 

ggplot(momentum %>% filter(rank > 60, rank <= 72), aes(x = date)) + 
  geom_line(aes(y = return_buyhold + 1), colour = "blue", size = 1.5) + 
  geom_line(aes(y = return_momentum_05d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_10d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_15d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_20d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_25d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_30d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_35d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_40d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_45d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_50d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_55d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_60d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_avg + 1), colour = "red", alpha = 0.8) + 
  scale_y_continuous(trans = "log2") +
  facet_wrap(~ name_sorted, ncol = 4, scales = "free_y") 

ggplot(momentum %>% filter(id == "bitcoin"), aes(x = date)) + 
  geom_line(aes(y = return_buyhold + 1), colour = "blue", size = 1.5) + 
  geom_line(aes(y = return_momentum_05d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_10d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_15d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_20d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_25d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_30d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_35d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_40d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_45d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_50d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_55d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_60d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_avg + 1), colour = "red", alpha = 0.8) + 
  scale_y_continuous(trans = "log2") +
  facet_wrap(~ name_sorted, ncol = 4, scales = "free_y") 

ggplot(momentum %>% filter(id == "bitcoin"), aes(x = date)) + 
  geom_line(aes(y = close, colour = signal_avg), size = 1.5) + 
  scale_y_continuous(trans = "log2") + 
  scale_colour_gradient(low = "red", high = "blue")

ggplot(momentum %>% filter(id == "ethereum"), aes(x = date)) + 
  geom_line(aes(y = return_buyhold + 1), colour = "blue", size = 1.5) + 
  geom_line(aes(y = return_momentum_05d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_10d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_15d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_20d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_25d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_30d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_35d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_40d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_45d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_50d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_55d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_60d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_avg + 1), colour = "red", alpha = 0.8) + 
  scale_y_continuous(trans = "log2") +
  facet_wrap(~ name_sorted, ncol = 4, scales = "free_y") 

ggplot(momentum %>% filter(id == "ethereum"), aes(x = date)) + 
  geom_line(aes(y = close, colour = signal_avg), size = 1.5) + 
  scale_y_continuous(trans = "log2") + 
  scale_colour_gradient(low = "red", high = "blue")

ggplot(momentum %>% filter(id == "ripple"), aes(x = date)) + 
  geom_line(aes(y = return_buyhold + 1), colour = "blue", size = 1.5) + 
  geom_line(aes(y = return_momentum_05d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_10d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_15d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_20d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_25d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_30d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_35d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_40d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_45d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_50d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_55d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_60d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_avg + 1), colour = "red", alpha = 0.8) + 
  scale_y_continuous(trans = "log2") +
  facet_wrap(~ name_sorted, ncol = 4, scales = "free_y") 

ggplot(momentum %>% filter(id == "ripple"), aes(x = date)) + 
  geom_line(aes(y = close, colour = signal_avg), size = 1.5) + 
  scale_y_continuous(trans = "log2") + 
  scale_colour_gradient(low = "red", high = "blue")

ggplot(momentum %>% filter(id == "litecoin"), aes(x = date)) + 
  geom_line(aes(y = return_buyhold + 1), colour = "blue", size = 1.5) + 
  geom_line(aes(y = return_momentum_05d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_10d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_15d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_20d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_25d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_30d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_35d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_40d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_45d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_50d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_55d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_60d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_avg + 1), colour = "red", alpha = 0.8) + 
  scale_y_continuous(trans = "log2") +
  facet_wrap(~ name_sorted, ncol = 4, scales = "free_y") 

ggplot(momentum %>% filter(id == "litecoin"), aes(x = date)) + 
  geom_line(aes(y = close, colour = signal_avg), size = 1.5) + 
  scale_y_continuous(trans = "log2") + 
  scale_colour_gradient(low = "red", high = "blue")

ggplot(momentum %>% filter(id == "dash"), aes(x = date)) + 
  geom_line(aes(y = return_buyhold + 1), colour = "blue", size = 1.5) + 
  geom_line(aes(y = return_momentum_05d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_10d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_15d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_20d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_25d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_30d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_35d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_40d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_45d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_50d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_55d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_60d + 1), colour = "grey", alpha = 0.8) + 
  geom_line(aes(y = return_momentum_avg + 1), colour = "red", alpha = 0.8) + 
  scale_y_continuous(trans = "log2") +
  facet_wrap(~ name_sorted, ncol = 4, scales = "free_y") 

ggplot(momentum %>% filter(id == "dash"), aes(x = date)) + 
  geom_line(aes(y = close, colour = signal_avg), size = 1.5) + 
  scale_y_continuous(trans = "log2") + 
  scale_colour_gradient(low = "red", high = "blue")

#' # 7. Table
table <- momentum %>% 
  group_by(id) %>% 
  filter(date == max(date)) %>% 
  select(rank, id, return_buyhold, return_momentum_avg) %>% 
  mutate(return_alpha = round((return_momentum_avg - return_buyhold) / abs(return_buyhold), 2))
knitr::kable(table[1:100, ])
