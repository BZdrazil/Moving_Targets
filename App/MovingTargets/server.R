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
  
  disease_aggregate <- reactive({
    case_when(
      input$disease_aggregate == 'Protein Family' ~ 'protein_family',
      input$disease_aggregate == 'Protein Class' ~ 'protein_class',
      input$disease_aggregate == 'GO Biological Process' ~ 'go_bp'
    )
  })
  
  gobp_aggregate <- reactive({
    case_when(
      input$gobp_aggregate == 'Protein Family' ~ 'protein_family',
      input$gobp_aggregate == 'Protein Class' ~ 'protein_class'
    )
  })
  
  target_year_range <- reactive({
    input$target_year
  })
  disease_year_range <- reactive({
    input$disease_year
  })
  
  
  smooth_trends <- reactive({
    if ('do_smooth_gobp' %in% names(input)) gobp <- input$do_smooth_gobp
    else gobp <- FALSE
    if ('do_smooth_target' %in% names(input)) target <- input$do_smooth_target
    else target <- FALSE
    if ('do_smooth_disease' %in% names(input)) disease <- input$do_smooth_disease
    else disease <- FALSE
    
    list(gobp=gobp,
         target=target,
         disease=disease)
  })
  
  data_gene <- reactive({
    subset(by_target, gene %in% input$target_gene & 
             year >= input$target_year[1] & year <= input$target_year[2])
  })
  
  data_gobp <- reactive({
    subset(by_go_bp, go_bp %in% input$gobp & 
             year >= input$gobp_year[1] & year <= input$gobp_year[2])  
  })
  
  data_disease <- reactive({
    subset(by_disease, diseaseName %in% input$disease  & 
             year >= input$disease_year[1] & year <= input$disease_year[2])
  })
  
  data_gobp_targetclass <- reactive({
    if (is.null(input$gobp) | length(input$gobp) != 1) 
      return(data.frame())
    t1 <- target_go %>% 
      dplyr::filter(go_term == input$gobp)
    agg <- gobp_aggregate()

    if (agg == 'protein_family') {
      t1 <- t1 %>% 
        left_join(by_target, by='chembl_id') %>% 
        dplyr::select(year, n_act, group=protein_family)
    } else if (agg == 'protein_class') {
      t1 <- t1 %>% 
        left_join(by_target, by='chembl_id') %>% 
        dplyr::select(year, protein_class=protein_class_pref_name) %>% 
        left_join(by_protein_class) %>% 
        dplyr::select(year, n_act, group = protein_class)
    } 
    
    t1 <- t1 %>% 
      dplyr::filter(year >= input$gobp_year[1] & year <= input$gobp_year[2] )
    t2 <- t1 %>% group_by(year) %>% summarize(year_total = sum(n_act))
    t1 <- t1 %>% left_join(t2) %>% mutate(p_act = 100*n_act/year_total)
    
    print(t1)
    # per year, aggregate all terms that have < 10% into 'Other'
    t1 <- t1 %>% 
      group_by(year) %>% 
      mutate(group = ifelse(p_act < 10, 'Other', group)) %>% 
      ungroup() %>% 
      group_by(year, group) %>% 
      summarize(p_act = sum(p_act))
    
    return(t1)
  })
  
  data_disease_targetclass <- reactive({
    if (is.null(input$disease) | length(input$disease) != 1) 
      return(data.frame())

    t1 <- target_disease %>% 
      dplyr::filter(disease == input$disease)
    
    agg <- disease_aggregate()
    if (agg == 'protein_family') {
      t1 <- t1 %>% 
        left_join(by_target, by='chembl_id') %>% 
        dplyr::select(year, n_act, group=protein_family)
    } else if (agg == 'protein_class') {
      t1 <- t1 %>% 
        left_join(by_target, by='chembl_id') %>% 
        dplyr::select(year, protein_class=protein_class_pref_name) %>% 
        left_join(by_protein_class) %>% 
        dplyr::select(year, n_act, group = protein_class)
    } else if (agg == 'go_bp') {
      t1 <- t1 %>% 
        left_join(target_go) %>% 
        left_join(by_go_bp, by=c('go_term'='go_bp')) %>% 
        dplyr::select(year, n_act, group = go_term)
    }
    
    t1 <- t1 %>% 
      dplyr::filter(year >= input$disease_year[1] & year <= input$disease_year[2] )
    t2 <- t1 %>% group_by(year) %>% summarize(year_total = sum(n_act))
    t1 <- t1 %>% left_join(t2) %>% mutate(p_act = 100*n_act/year_total)
    
    # per year, aggregate all terms that have < 10% into 'Other'
    t1 <- t1 %>% 
      group_by(year) %>% 
      mutate(group = ifelse(p_act < 0.1, 'Other', group)) %>% 
      ungroup() %>% 
      group_by(year, group) %>% 
      summarize(p_act = sum(p_act))
    
    return(t1)
  })
  
  
  vis_target <- reactive({
    d <- data_gene()
    if (nrow(d) == 0) return(get_null_plot())
    
    figure <- d %>% 
      ggvis(~year, ~n_act) %>% 
      group_by(gene)
    
    if (smooth_trends()$target) {
      figure <- figure %>% layer_smooths(stroke = ~factor(gene) )
    } else {
      figure <- figure %>% layer_lines(stroke = ~factor(gene) )
    }
    figure %>% 
      add_axis("x", title = "Publication Year", format='####') %>% 
      add_axis("y", title = "Number of Activity Measurements")  %>% 
      add_legend(scales="stroke", title="") %>% 
      set_options(width=800)
  })

  vis_target_pmid <- reactive({
    d <- data_gene()
    if (nrow(d) == 0) return(get_null_plot())
    
    figure <- d %>% 
      ggvis(~year, ~n_doc) %>% 
      group_by(gene)
    
    if (smooth_trends()$target) {
      figure <- figure %>% layer_smooths(stroke = ~factor(gene) )
    } else {
      figure <- figure %>% layer_lines(stroke = ~factor(gene) )
    }
    figure %>% 
      add_axis("x", title = "Year", format='####') %>% 
      add_axis("y", title = "Number of Publications")  %>% 
      add_legend(scales="stroke", title="") %>% 
      set_options(width=800)
  })

    
  vis_disease <- reactive({
    d <- data_disease()
    if (nrow(d) == 0) return(get_null_plot())
    
    figure <- d %>% 
      ggvis(~year, ~n_act) %>% 
      group_by(diseaseName)
    
    if (smooth_trends()$disease) {
      figure <- figure %>% layer_smooths(stroke = ~factor(diseaseName) )
    } else {
      figure <- figure %>% layer_lines(stroke = ~factor(diseaseName) )
    }
  
    figure %>% 
      add_axis("x", title = "Year", format='####') %>% 
      add_axis("y", title = "Number of Bioactivities")  %>% 
      add_legend(scales="stroke", title="") %>% 
      set_options(width=800)
    
    
  })
  
  vis_disease_target <- reactive({
    d <- data_disease_targetclass()
    if (nrow(d) == 0) return(get_null_plot())
    d %>% 
      ggvis(~year, ~p_act, fill=~group) %>%
      group_by(group) %>%
      layer_bars()  %>%
      add_axis("x", title = "Year", format='####') %>%
      add_axis("y", title = "% Bioactivities")  %>%
      add_legend(scales="fill", title="") %>% 
      set_options(width=800)
  })
  
  vis_gobp_target <- reactive({
    d <- data_gobp_targetclass()
    if (nrow(d) == 0) return(get_null_plot())
    d %>% 
      ggvis(~year, ~p_act, fill=~group) %>%
      group_by(group) %>%
      layer_bars()  %>%
      add_axis("x", title = "Year", format='####') %>%
      add_axis("y", title = "% Bioactivities")  %>%
      add_legend(scales="fill", title="") %>% 
      set_options(width=800)
  })
  
  vis_gobp <- reactive({
    d <- data_gobp()
    if (nrow(d) == 0) return(get_null_plot())
    
    figure <- d %>% 
      ggvis(~year, ~n_act) %>% 
      group_by(go_bp)
    
    if (smooth_trends()$gobp) {
      figure <- figure %>% layer_smooths(stroke = ~factor(go_bp))
    } else {
      figure <- figure %>% layer_lines(stroke = ~factor(go_bp) )
    }
    figure %>% 
      add_axis("x", title = "Year", format='####') %>% 
      add_axis("y", title = "Number of Bioactivities")  %>% 
      add_legend(scales="stroke", title="") %>% 
      set_options(width=800)
  })
  
  vis_target %>% bind_shiny("target_trend_plot")
  vis_target_pmid %>% bind_shiny("target_pmid_trend_plot")
  vis_disease %>% bind_shiny("disease_trend_plot")
  vis_disease_target %>% bind_shiny("disease_trend_barchart")
  vis_gobp %>% bind_shiny("gobp_trend_plot")
  vis_gobp_target %>% bind_shiny("gobp_trend_barchart")
  
})
