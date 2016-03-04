### per recruit analysis
### teaching tool
### Merrill Rudd
### March 2016


library(shiny)

shinyUI(fluidPage(
  
  #  Application title
  titlePanel("Per Recruit Analysis"),
  
  tabsetPanel(
    tabPanel("Growth",
             sidebarLayout(
               sidebarPanel(
                 numericInput("amax", "Age classes:", value=25), 
                 numericInput("Linf", withMathJax("$$L_\\infty:$$"), value=100),
                 numericInput("k", "k:", value = 0.1),
                 numericInput("t0", withMathJax("$$t_0:$$"), value = 0),
                 numericInput("b", "Weight-Length exponent (b):", value = 3),
                 numericInput("d", "Allometric scaling (d):",value = 0.0067)
                 ),
                mainPanel(
                  column(5, plotOutput("VBGFplot")),
                  column(5, plotOutput("WeightAtAge"))
                  )
               )
             
             ),
    tabPanel("Selectivity and Maturity",
             sidebarLayout(
               sidebarPanel(
                 numericInput("aselex", "Age at Selectivity",  value=7),
                 numericInput("amat", "Age at Maturity",value=7),
                 numericInput("surv", "Survival (S)",value=0.8),
                 numericInput("u", "Exploitation Rate (u)", value=0.3)
                 ),
               mainPanel(
                 column(5, plotOutput("SelexAtAge"), plotOutput("MatureAtAge")),
                 column(5, plotOutput("NumbersAtAge"), plotOutput("FecundityAtAge"))
                 )
               )
             ),
    tabPanel("Per Recruit",
             sidebarLayout(
               sidebarPanel(
                 strong(textOutput("SBPRf")),
                 strong(textOutput("SBPR0")),
                 br(),
                 strong(textOutput("SPR"), 
                 br(),
                 numericInput("Fref", "Target spawning biomass (% of unfished)", value=30))
                 ),
               mainPanel(
                 plotOutput("SBPRplot"),
                 plotOutput("Kobe")
                 )
               )
             )
    )
  
))
