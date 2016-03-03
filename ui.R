### per recruit analysis
### teaching tool
### Merrill Rudd
### March 2016


library(shiny)

shinyUI(fluidPage(
  
  #  Application title
  titlePanel("Per Recruit Analysis"),
  
  # Sidebar with sliders that demonstrate various available
  # options

    sidebarLayout(
      sidebarPanel(
             sliderInput("amax", "Age classes:", 
                         min=1, max=100, value=50),    
             sliderInput("Linf", withMathJax("$$L_\\infty:$$"), 
                         min=1, max=1000, value=100),
             sliderInput("k", "k:", 
                         min = 0.01, max = 1, value = 0.1,step=0.01),
             sliderInput("t0", withMathJax("$$t_0:$$"),
                         min = -5, max = 5, value = 0,step=0.1),
             
             sliderInput("b", "Weight-Length exponent (b):", 
                         min = 2, max = 4, value = 3,step=0.01),
             
             sliderInput("d", "Allometric scaling (d):",
                         min = 0.0001, max = 0.01, value = 0.0067,step=0.0001),
             
             sliderInput("aselex", "Age at Selectivity", 
                         min=1, max=100, value=7, step=1),
             
             sliderInput("amat", "Age at Maturity",
                         min=0, max=100, value=7, step=0.5),
             
             sliderInput("surv", "Survival (S)",
                         min=0, max=1, step=0.01, value=0.8),
             
             sliderInput("u", "Exploitation Rate (u)",
                         min=0, max=1, step=0.01, value=0.3)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Per Recruit",
                   column(3, strong(textOutput("SBPRf"))),
                   column(3, strong(textOutput("SBPR0"))),
                   column(3, strong(textOutput("SPR"))),
                   br(),
                   column(5,plotOutput("SpawnBioAtAge")),
                   column(5, plotOutput("CatchAtAge"))
          ),
          tabPanel("Growth", 
                 column(5, plotOutput("VBGFplot")),
                 column(5, plotOutput("WeightAtAge"))
          ),
          tabPanel("Selectivity and Maturity",
                   column(5, plotOutput("SelexAtAge"), plotOutput("MatureAtAge")),
                   column(5, plotOutput("NumbersAtAge"), plotOutput("FecundityAtAge"))
          )
        )
      )

      )

))