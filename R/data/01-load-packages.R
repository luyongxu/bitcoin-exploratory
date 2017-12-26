#' ---
#' title: "Load Data Packages and Functions"
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
library(tidyverse)
library(purrr)
library(stringr)
library(lubridate)
library(rvest)
library(httr)
library(DT)
library(jsonlite)
library(Quandl)
library(gtrendsR)

