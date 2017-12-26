#' ---
#' title: "Scrape Wikipedia Views"
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

#' # 2. Download Wikipedia Data 
#' Data is scraped from https://tools.wmflabs.org/pageviews/?project=en.wikipedia.org&
#' platform=all-access&agent=user&range=all-time&pages=Bitcoin
scrape_wikipedia <- function(project, access, agent, article, granularity, start, end) { 
  base_url <- "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/"
  df <- fromJSON(str_c(base_url, project, "/", access, "/", agent, "/", URLencode(article), 
                       "/", granularity, "/", start, "/", end)) %>% 
    .[["items"]] %>% 
    mutate(date = as.Date(as.POSIXct(timestamp, format = "%Y%m%d%H", tz = "GMT"))) %>%
    as_tibble()
  return(df)
}

#' # 3. Define List of Articles to Scrape 
#' This article list contains the top 20 projects as determined by the Wikipedia pageviews API: 
#' https://tools.wmflabs.org/langviews/?project=en.wikipedia.org&platform=all-access&agent=user&
#' range=all-time&sort=views&direction=1&view=list&page=Bitcoin
article_list <- list(c("en.wikipedia.org", "Bitcoin"), 
                     c("ru.wikipedia.org", "Биткойн"), 
                     c("de.wikipedia.org", "Bitcoin"), 
                     c("es.wikipedia.org", "Bitcoin"), 
                     c("zh.wikipedia.org", "比特币"), 
                     c("ja.wikipedia.org", "ビットコイン"), 
                     c("fr.wikipedia.org", "Bitcoin"), 
                     c("it.wikipedia.org", "Bitcoin"), 
                     c("pt.wikipedia.org", "Bitcoin"), 
                     c("vi.wikipedia.org", "Bitcoin"), 
                     c("pl.wikipedia.org", "Bitcoin"), 
                     c("ar.wikipedia.org", "بيتكوين"), 
                     c("nl.wikipedia.org", "Bitcoin"), 
                     c("ko.wikipedia.org", "비트코인"), 
                     c("cs.wikipedia.org", "Bitcoin"), 
                     c("id.wikipedia.org", "Bitcoin"), 
                     c("tr.wikipedia.org", "Bitcoin"), 
                     c("uk.wikipedia.org", "Bitcoin"), 
                     c("hu.wikipedia.org", "Bitcoin"), 
                     c("sv.wikipedia.org", "Bitcoin"))

#' # 4. Scrape Bitcoin Wikipedia Page Views 
#' Each article is appended to a list and then combined into one single dataframe.
bitcoin_wiki <- tibble()
for (i in seq_along(article_list)) { 
  df <- scrape_wikipedia(project = article_list[[i]][[1]], 
                         access = "all-access", 
                         agent = "user", 
                         article = article_list[[i]][[2]], 
                         granularity = "daily", 
                         start = "20150701", 
                         end = format(Sys.Date(), "%Y%m%d")) %>% 
    select(project, date, views)
  bitcoin_wiki <- bitcoin_wiki %>% bind_rows(df)
}

#' # 5. Combine Top 20 Projects
#' Aggreagte page views from top 20 projects and create a new project labeled Top 20. 
bitcoin_wiki_all <- bitcoin_wiki %>% 
  group_by(date) %>% 
  summarise(views = sum(views)) %>% 
  mutate(project = "Top 20")
bitcoin_wiki <- bitcoin_wiki %>% 
  bind_rows(bitcoin_wiki_all)

#' # 5. Print and Plot 
print(bitcoin_wiki)
ggplot(bitcoin_wiki, aes(x = date, y = views)) + 
  geom_line(colour = "blue") + 
  facet_wrap(~ reorder(project, -views)) + 
  scale_y_continuous(trans = "log2")

#' # 6. Clean
rm(df, article_list, i, scrape_wikipedia, bitcoin_wiki_all)
