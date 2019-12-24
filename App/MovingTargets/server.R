#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source('funcs.R')
load("datasets.Rda")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  data_gene <- reactive({
    subset(by_target, gene %in% input$target_gene & 
             year >= input$target_year[1] & year <= input$target_year[2])
  })
  
  data_disease <- reactive({
    subset(by_disease, diseaseName %in% input$disease  & 
             year >= input$disease_year[1] & year <= input$disease_year[2])
  })
  
  data_disease_targetclass <- reactive({
    if (is.null(input$disease) | length(input$disease) != 1) 
      return(data.frame())
    t1 <- target_disease %>% 
      dplyr::filter(disease == input$disease) %>% 
      left_join(by_target, by='chembl_id') %>% 
      dplyr::select(year, n_act, protein_family) %>% 
      dplyr::filter(year >= input$disease_year[1] & year <= input$disease_year[2] )
    t2 <- t1 %>% group_by(year) %>% summarize(year_total = sum(n_act))
    t1 <- t1 %>% left_join(t2) %>% mutate(p_act = n_act/year_total)
    return(t1)
  })
  
  smooth_trends <- reactive({
    input$do_smooth_target
  })
  
  target_year_range <- reactive({
    input$target_year
  })
  disease_year_range <- reactive({
    input$disease_year
  })
  
  get_null_plot <- function() {
    data.frame(year=0, n_act=0) %>% 
             ggvis(~year, ~n_act) %>% 
             add_axis("x", title = "Publication Year", format="####") %>% 
             add_axis("y", title = "Number of Bioactivities") 
  }
  
  vis_target <- reactive({
    d <- data_gene()
    if (nrow(d) == 0) return(get_null_plot())
    
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
      add_axis("y", title = "Number of Activity Measurements")  %>% 
      add_legend(scales="stroke", title="")
  })
  
  vis_disease <- reactive({
    d <- data_disease()
    if (nrow(d) == 0) return(get_null_plot())
    
    if (smooth_trends() && !is.null(d) && nrow(d) > 0) {
      d <- do.call(rbind, by(d, d$diseaseName, function(x) {
        x$n_act <- smooth(x$n_act, kind="3")
        return(x)
      }))
    }
    
    d %>% 
      ggvis(~year, ~n_act) %>% 
      group_by(diseaseName) %>% 
      layer_lines(stroke = ~factor(diseaseName) ) %>% 
      # layer_model_predictions(model = "MASS:rlm", formula=n_act~year, se = TRUE) %>%
      add_axis("x", title = "Publication Year", format='####') %>% 
      add_axis("y", title = "Number of Bioactivities")  %>% 
      add_legend(scales="stroke", title="")
    
  })
  
  vis_disease_target <- reactive({
    d <- data_disease_targetclass()
    if (nrow(d) == 0) return(get_null_plot())
    d %>% 
      ggvis(~year, ~p_act, fill=~protein_family) %>%
      group_by(protein_family) %>%
      layer_bars()  %>%
      add_axis("x", title = "Publication Year", format='####') %>%
      add_axis("y", title = "% Bioactivities")  %>%
      add_legend(scales="fill", title="")
    
  })
  
  vis_target %>% bind_shiny("target_trend_plot")
  vis_disease %>% bind_shiny("disease_trend_plot")
  vis_disease_target %>% bind_shiny("disease_trend_barchart")
  
  # subplot <- reactive({
  # if (nrow(data_disease_targetclass()) > 0) {
  #   data_disease_targetclass() %>% 
  #     ggvis(~year, ~n_act, fill=~protein_family) %>% 
  #     group_by(protein_family) %>% 
  #     layer_bars()  %>% 
  #     add_axis("x", title = "Publication Year", format='####') %>% 
  #     add_axis("y", title = "% Bioactivities")  %>% 
  #     add_legend(scales="stroke", title="")
  # } else (return(get_null_plot()))
  # })
  # 
  # subplot %>% bind_shiny("target_trend_plot")
  #   
})
