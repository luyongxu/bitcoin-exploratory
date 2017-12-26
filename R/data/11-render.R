#' ---
#' title: "Render Data"
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

#' # 1. Render Functions
render_wrapper <- function(filename, output) { 
  if (output == "html_document") { 
    rmarkdown::render(input = paste0("./Data/", filename, ".R"), 
                      output_file = paste0("./notebooks/data/", filename, ".html"), 
                      knit_root_dir = ".", 
                      output_format = "html_document")
  }
  if (output == "pdf_document") { 
    rmarkdown::render(input = paste0("./Data/", filename, ".R"), 
                      output_file = paste0("./notebooks/data/", filename, ".pdf"), 
                      knit_root_dir = ".", 
                      output_format = "pdf_document")
  }
  if (output == "flex_dashboard") { 
    rmarkdown::render(input = paste0("./Data/", filename, ".Rmd"), 
                      output_file = paste0("./notebooks/data/", filename, ".html"), 
                      knit_root_dir = ".", 
                      output_format = "flexdashboard::flex_dashboard")
  }
}

#' # 2. Render Research
render_wrapper("01-load-packages", "html_document")
render_wrapper("02-scrape-bitcoin-price", "html_document")
render_wrapper("03-scrape-wikipedia-views", "html_document")
render_wrapper("04-scrape-coinmarketcap", "html_document")
render_wrapper("05-load-coinmarketcap", "html_document")
render_wrapper("06-scrape-google-trends-daily", "html_document")
render_wrapper("07-clean-google-trends-daily", "html_document")
render_wrapper("08-load-google-trends-daily", "html_document")
render_wrapper("09-scrape-github-commits", "html_document")
render_wrapper("10-scrape-reddit", "html_document")
