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
  titlePanel(sprintf("Moving Targets (v%s)",get_version())),
  titlePanel(div(style="color:grey;font-size:large;font-style:italic;", 
        "Exploring target and disease trends")),
  
  fluidRow(
    tabsetPanel(
    type = 'tabs',
    tabPanel("Target View",
            div(style="padding-top:1em;"),
             column(3, wellPanel(
                 sliderInput("target_year","Publication Date",
                   minYear, maxYear, step = 1, sep = '',
                   value = c(minYear, maxYear)
                 ),
                 selectizeInput("target_gene","Gene",
                   choices = sort(unique(by_target$gene)),
                   selected = NULL,multiple = TRUE, 
                   options = list(placeholder = 'select gene symbols')
                 )
               )),
             column(9, ggvisOutput('target_trend_plot'), ggvisOutput('target_pmid_trend_plot'))),
    
    tabPanel("Disease View",
             div(style="padding-top:1em;"),
             column(3, wellPanel(
                 sliderInput("disease_year","Publication Date", 
                             minYear,maxYear,step = 1,sep = '',
                             value = c(minYear, maxYear)),
                 selectizeInput("disease","Disease",
                   choices = sort(unique(by_disease$diseaseName)),
                   selected = NULL,multiple = TRUE,
                   options = list(placeholder = 'select disease')),
                 selectInput("disease_aggregate", "Aggregate by",
                                choices = c('Protein Family',
                                            'Protein Class',
                                            'GO Biological Process'),
                                selected = 'Protein Family', multiple=FALSE)
               )),
             column(9, ggvisOutput('disease_trend_plot'),
                    ggvisOutput("disease_trend_barchart"))
             ),
    
    tabPanel("Gene Ontology View",
             div(style="padding-top:1em;"),
             column(3, wellPanel(
               sliderInput("gobp_year","Publication Date", 
                           minYear,maxYear,step = 1,sep = '',
                           value = c(minYear, maxYear)),
               selectizeInput("gobp","GO Biological Process",
                              choices = sort(unique(by_go_bp$go_bp)),
                              selected = NULL,multiple = TRUE,
                              options = list(placeholder = 'select GO BP term'))
             )),
             column(9, ggvisOutput('gobp_trend_plot'))
    ),
    
    tabPanel("About", div(style="margin:10px;",includeMarkdown("about.md")))
  ))
  
)
