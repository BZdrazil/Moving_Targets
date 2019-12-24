#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source('funcs.R')
load("datasets.Rda")

minYear <- min(c(by_disease$year,
                 by_protein_family$year,
                 by_target$year))
maxYear <- max(c(by_disease$year,
                 by_protein_family$year,
                 by_target$year))

# Define UI for application that draws a histogram
fluidPage(
  useShinyalert(),
  
  # Application title
  titlePanel(
    sprintf(
      "Moving Targets (v%s) - Explore trends in targets and diseases",
      get_version()
    )
  ),
  
  fluidRow(tabsetPanel(
    type = 'tabs',
    tabPanel("Target View",
             column(3, wellPanel(
                 sliderInput("target_year","Publication Date",
                   minYear, maxYear, step = 1, sep = '',
                   value = c(minYear, maxYear)
                 ),
                 selectizeInput("target_gene","Gene",
                   choices = sort(unique(by_target$gene)),
                   selected = NULL,multiple = TRUE, 
                   options = list(placeholder = 'select gene symbols')
                 ),
                 checkboxInput("do_smooth_target", "Smooth trends", value =
                                 TRUE)
               )),
             column(9, ggvisOutput('target_trend_plot'))),
    tabPanel("Disease View",
             column(3, wellPanel(
                 sliderInput("disease_year","Publication Date", 
                             minYear,maxYear,step = 1,sep = '',
                             value = c(minYear, maxYear)),
                 selectizeInput("disease","Disease",
                   choices = sort(unique(by_disease$diseaseName)),
                   selected = NULL,multiple = TRUE,
                   options = list(placeholder = 'select disease')),
                 checkboxInput("do_smooth", "Smooth trends", value = TRUE)
               )),
             column(9, ggvisOutput('disease_trend_plot'),
                    ggvisOutput("disease_trend_barchart"))
             ),
    tabPanel("About", includeMarkdown("about.md"))
  ))
  
)
