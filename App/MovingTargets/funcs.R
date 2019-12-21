library(shiny)
library(shinyalert)
library(markdown)
library(data.table)
library(dplyr)
library(reshape2)
library(ggvis)
library(DT)

get_version <- function() {
  return("0.2.3")
}