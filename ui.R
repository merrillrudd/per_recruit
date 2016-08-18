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
                 numericInput("amax", "Age classes:", value=23), 
                 numericInput("Linf", "Linf:", value=64.58),
                 numericInput("k", "k:", value = 0.21),
                 numericInput("t0", "t0:", value = -0.01),
                 numericInput("b", "Weight-Length exponent:", value = 2.79),
                 numericInput("d", "Allometric scaling:",value = 0.0245)
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
                 numericInput("aselex", "Age at Selectivity",  value=3),
                 numericInput("amat", "Age at Maturity",value=4),
                 numericInput("M", "Natural Mortality",value=0.43),
                 numericInput("fish", "Fishing Mortality", value=0.1)
                 ),
               mainPanel(
                 column(5, plotOutput("SelexMature")),
                 column(5, plotOutput("NumbersAtAge"))
                 )
               )
             ),
    tabPanel("Per Recruit",
             sidebarLayout(
               sidebarPanel(
                 strong(textOutput("SPR"), 
                 br(),
                 numericInput("Fref", "Target spawning biomass (% of unfished)", value=30))
                 ),
               mainPanel(
                 column(5, plotOutput("SBPRplot")),
                 column(5, plotOutput("Kobe"))
                 )
               )
             )
    )
  
))
