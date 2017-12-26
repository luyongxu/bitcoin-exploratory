#' ---
#' title: "Automate Simple Momentum"
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

#' # 1. Load Packages
library(dplyr)
library(gmailr)
library(rmarkdown)
library(mailR)

#' # 2. Render 
render(input = "./research/06-research-simple-omentum.R", 
       output_file = "./notebooks/research/06-research-simple-momentum.html", 
       knit_root_dir = ".", 
       output_format = "html_document")

#' # 3. Email
send.mail(from = "luyongxur@gmail.com", 
          to = "luyongxu@gmail.com", 
          subject = "Test", 
          body = "This is a test.", 
          attach.files = "", 
          smtp = list(host.name = "smtp.gmail.com", 
                      port = 465, 
                      user.name = "luyongxur@gmail.com", 
                      passwd = "", 
                      ssl = TRUE), 
          authenticate = TRUE, 
          send = TRUE)
