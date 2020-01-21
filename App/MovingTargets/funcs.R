library(shiny)
library(shinyalert)
library(markdown)
library(data.table)
library(dplyr)
library(reshape2)
library(ggvis)
library(DT)

get_version <- function() {
  return("0.3.4")
}



get_null_plot <- function() {
  data.frame(year=0, n_act=0) %>% 
    ggvis(~year, ~n_act) %>% 
    add_axis("x", title = "Publication Year", format="####") %>% 
    add_axis("y", title = "% Bioactivities") 
}


