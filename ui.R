library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  
  theme = shinytheme("spacelab"),
  
  titlePanel("Statistics and Power Analysis in Infancy Research"),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      sliderInput("m", p("Mean looking time (M)"), 
                  min = 2, max = 60, value = 8, step = 1),
      sliderInput("sd", p("Standard deviation (SD)"), 
                  min = 1, max = 15, value = 2, step = 1),
      sliderInput("d", p("Effect size (Cohen's d)"),
                  min = 0, max = 2.5, value = .5, step = .1),
      sliderInput("N", p("Number of infants per group (N)"),
                  min = 4, max = 120, value = 16, step = 2),
      radioButtons("control", p("Conditions"),
                  choices = list("Experimental only" = FALSE, 
                                 "Experimental + negative control" = TRUE)),
      radioButtons("interval", p("Type of error bars"),
                     choices = list("Standard error of the mean" = "sem", 
                                    "95% confidence interval" = "ci"), 
                   selected = "ci"),
      actionButton("go", label = "Sample Again")      
    ), 
    
    mainPanel(
      width = 8,
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tabsetPanel(
        tabPanel("Bar Plot", 
                 h4("Randomly sampled data"),
                 plotOutput("bar"),
                 h4("Statistical tests"),
                 textOutput("stat")), 
        tabPanel("By-Infant Plot", 
                 h4("Randomly sampled data"),
                 plotOutput("scatter")),
        tabPanel("Power Analysis", plotOutput("power"), 
                 br(),
                 p("Plot shows statistical power to detect a difference between 
                    conditions at p < .05 with the selected effect size, 
                    plotted by conditions. Red dot shows power with currently 
                    selected N. Dashed lines show 80% power and necessary sample size."))
        )
      )
    )
  )
)