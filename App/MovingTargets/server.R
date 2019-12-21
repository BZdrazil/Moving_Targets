#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(data.table)
library(dplyr)
library(ggvis)

library(shiny)
library(shinyalert)

load("datasets.Rda")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  data_gene <- reactive({
    subset(by_target, gene %in% input$target_gene & 
             year >= input$year[1] & year <= input$year[2])
  })
  
  data_disease <- reactive({
    subset(by_disease, diseaseName %in% input$disease  & 
             year >= input$year[1] & year <= input$year[2])
  })
  
  smooth_trends <- reactive({
    input$do_smooth
  })
  
  year_range <- reactive({
    input$year
  })
  
  vis <- reactive({
    d <- data_gene()
    dd <- data_disease()
    
    if ( (nrow(d) == 0 & nrow(dd) == 0) ||
         (nrow(d) > 0 & nrow(dd) > 0) ){
      if (nrow(d) > 0 & nrow(dd) > 0) {
        shinyalert("Error!", "Currently can't specify both a gene and a disease", 
                   type = "error")
      }
      return(data.frame(year=0, n_act=0) %>% 
               ggvis(~year, ~n_act) %>% 
               add_axis("x", title = "Publication Year", format="####") %>% 
               add_axis("y", title = "Number of Publications") )
    }
    
    if (nrow(d) > 0 && nrow(dd) == 0) {
    } else if (nrow(d)  == 0 && nrow(dd) >0) {
      d <- dd
      idx <- which(names(d) == 'diseaseName')
      names(d)[idx] <- 'gene'
    }
    
    # if (nrow(d) == 0) {
    #   return(data.frame(year=0, n_act=0, gene="") %>%
    #     ggvis(~year, ~n_act) %>%
    #     add_axis("x", title = "Publication Year", format="####") %>%
    #     add_axis("y", title = "Number of Publications"))
    # }
    
    if (smooth_trends() && !is.null(d) && nrow(d) > 0) {
      d <- do.call(rbind, by(d, d$gene, function(x) {
        x$n_act <- smooth(x$n_act, kind="3")
        return(x)
      })) 
    }
    
    d %>% 
      ggvis(~year, ~n_act) %>% 
      group_by(gene) %>% 
      layer_lines(stroke = ~factor(gene) ) %>% 
      # layer_model_predictions(model = "MASS:rlm", formula=n_act~year, se = TRUE) %>%
      add_axis("x", title = "Publication Year", format='####') %>% 
      add_axis("y", title = "Number of Publications")  %>% 
      add_legend(scales="stroke", title="")
  })
  
  vis %>% bind_shiny("trend_plot")
  
  
  
})
