#' ---
#' title: "Render Research"
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
    rmarkdown::render(input = paste0("./R/research/", filename, ".R"), 
                      output_file = paste0("./notebooks/research/", filename, ".html"), 
                      knit_root_dir = ".", 
                      output_format = "html_document")
  }
  if (output == "pdf_document") { 
    rmarkdown::render(input = paste0("./R/research/", filename, ".R"), 
                      output_file = paste0("./notebooks/research/", filename, ".pdf"), 
                      knit_root_dir = ".", 
                      output_format = "pdf_document")
  }
  if (output == "flex_dashboard") { 
    rmarkdown::render(input = paste0("./R/research/", filename, ".Rmd"), 
                      output_file = paste0("./notebooks/research/", filename, ".html"), 
                      knit_root_dir = ".", 
                      output_format = "flexdashboard::flex_dashboard")
  }
}

#' # 2. Render Research
render_wrapper("01-load-packages", "html_document")
render_wrapper("02-research-wikipedia", "html_document")
render_wrapper("03-research-wikipedia-dashboard", "flex_dashboard")
render_wrapper("04-research-prices", "html_document")
render_wrapper("05-research-prices-dashboard", "flex_dashboard")
render_wrapper("06-research-simple-momentum", "html_document")
render_wrapper("07-research-volatility", "html_document")
render_wrapper("08-research-google-trends", "html_document")
