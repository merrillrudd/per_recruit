# Define server logic required to draw figure
library(shiny)

shinyServer(
  function(input, output) 
  {    

################################
## population dynamic functions
################################
    ## length at age
get_lengths <-function(Linf,k,t0,b,d,ages)
{
  Lengths_exp<-Linf*(1-exp(-k*b*(1-d)*(ages-t0)))^(1/(b*(1-d)))
  return(Lengths_exp)
}

  ## selectivity at age
get_selex <- function(aselex, ages){
  Selex_a <- rep(0, length(ages))
  Selex_a[which(ages>=aselex)] <- 1
  return(Selex_a)
}

  ## numbers at age
get_Na <- function(ages, aselex, surv, exploit, R0){
  S_a <- get_selex(aselex, ages)
  N_a <- rep(NA, length(ages))
  N_a[1] <- R0
  for(i in 2:length(ages)){
    N_a[i] <- (1-S_a[i-1]*exploit)*surv*N_a[i-1]
  }
  return(N_a)
}

  ## weight at age
get_weight <- function(ages, Linf,k,t0,lwa, lwb){
  L_a <- get_lengths(Linf,k,t0,lwb,lwa,ages)
  W_a <- sapply(1:length(L_a), function(x) lwa*L_a[x]^lwb)
  return(W_a)
}


  ## proportion mature at age
get_mature <- function(agemat, ages){
  Mat_a <- rep(0, length(ages))
  Mat_a[which(ages>=agemat)] <- 1
  return(Mat_a)
}


  ## fecundity at age
get_fec <- function(agemat, ages, Linf, k, t0, lwa, lwb){
  Mat_a <- get_mature(agemat, ages)
  weight <- get_weight(ages, Linf, k, t0, lwa, lwb)
  Fec_a <- Mat_a*weight
  return(Fec_a)
}

  ## spawning biomass at age
get_SB <- function(ages, agemat, Linf, k, t0, lwa, lwb, aselex, surv, exploit, R0){
  N_a <- get_Na(ages, aselex, surv, exploit, R0)
  Fec_a <- get_fec(agemat, ages, Linf, k, t0, lwa, lwb)
  
  SB <- Fec_a*N_a
  return(SB)
}

  ## catch at age
get_catch <- function(ages, aselex, surv, exploit, R0, Linf,k,t0,lwa, lwb){
  N_a <- get_Na(ages, aselex, surv, exploit, R0)
  S_a <- get_selex(aselex, ages)
  weight <- get_weight(ages, Linf, k, t0, lwa, lwb)
  Cb_a <- N_a*S_a*exploit*weight
  return(Cb_a)
}

  ## spawning biomass per recruit, either in the fished or unfished conditions
get_SBPR <- function(agemat, Linf, k, t0, lwa, lwb, aselex, surv, exploit, R0, unfished){
  ages <- c(1:input$amax)
  SB <- get_SB(ages, agemat, Linf, k, t0, lwa, lwb, aselex, surv, exploit, R0)
  SBPR <- sum(SB)/R0
  if(unfished==FALSE) return(paste0("SBPR(fished) = ", round(SBPR,0)))
  if(unfished==TRUE) return(paste0("SBPR(unfished) = ", round(SBPR,0)))
}

  ## spawning potential ratio
get_SPR <- function(agemat, Linf, k, t0, lwa, lwb, aselex, surv, exploit, R0){
  ages <- c(1:input$amax)
  SBf <- get_SB(ages, agemat, Linf, k, t0, lwa, lwb, aselex, surv, exploit, R0)
  SBPRf <- sum(SBf)/R0
  
  SB0 <- get_SB(ages, agemat, Linf, k, t0, lwa, lwb, aselex, surv, 0, R0)
  SBPR0 <- sum(SB0)/R0
  
  return(paste0("SPR = ", round(SBPRf/SBPR0, 3)))
}

  ## yield per recruit
get_YPR <- function(aselex, surv, exploit, Linf,k,t0,lwa, lwb, R0)
{
  ages <- c(1:input$amax)
  Cb_a <- get_catch(ages, input$aselex, input$surv, input$u, 1, input$Linf, input$k, input$t0, input$d, input$b)
  YPR <- sum(Cb_a)/R0 
  return(paste0("YPR = ", round(YPR,0)))
}

################################
## output
################################

## length
output$VBGFplot <- renderPlot(
{
  ages<-c(1:input$amax)
  lengths.out<- get_lengths(input$Linf,input$k,input$t0,input$b,input$d,ages)
  # plot VBGF
  plot(ages, lengths.out, col = "steelblue",
       xlab="Age",ylab="Length (cm)",xlim=c(0,input$amax),
       ylim=c(0,input$Linf*1.1),type="l",lwd=5,
       main="Length at age")
}
    )

## numbers alive at age
output$NumbersAtAge <- renderPlot(
{
  ages <- c(1:input$amax)
  N_a <- get_Na(ages, input$aselex, input$surv, input$u, 1)
  ## plot numbers at age
  plot(ages, N_a, col = "darkgreen",
       xlab="Age",ylab="Numbers Alive",xlim=c(0,input$amax),
       ylim=c(0,1.1),type="l",lwd=5, main="Numbers at age")
  
}
  )

## weight at age
output$WeightAtAge <- renderPlot(
  {
    ages <- c(1:input$amax)
    W_a <- get_weight(ages, input$Linf, input$k, input$t0, input$d, input$b)
    plot(ages, W_a, col="navyblue", xlab="Age", ylab="Weight (g)",
         xlim=c(0, input$amax), ylim=c(0, max(W_a)*1.1), type="l", lwd=5,
         main="Weight At Age")
  })


## maturity at age
output$MatureAtAge <- renderPlot(
{
  ages <- c(1:input$amax)
  Mat_a <- get_mature(input$amat, ages)
  plot(ages, Mat_a, col="goldenrod",
       xlab="Age", ylab="Proportion Mature", xlim=c(0, input$amax),
       ylim=c(0, max(Mat_a)*1.1), type="l", lwd=5, main="Maturity at Age")
}
  )

## fecundity at age
output$FecundityAtAge <- renderPlot(
  {
    ages <- c(1:input$amax)
    Fec_a <- get_fec(input$amat, ages, input$Linf, input$k, input$t0, input$d, input$b)
    plot(ages, Fec_a, col = "goldenrod4",
         xlab="Age", ylab="Fecundity", xlim=c(0, input$amax),
         ylim=c(0,max(Fec_a)*1.1), type="l", lwd=5, main="Fecundity at age")
  })


## selectivity at age
output$SelexAtAge <- renderPlot(
{
  ages <- c(1:input$amax)
  S_a <- get_selex(input$aselex, ages)
  plot(ages, S_a, col = "forestgreen",
       xlab="Age", ylab="Selectivity", xlim=c(0, input$amax),
       ylim=c(0,max(S_a)*1.1), type="l", lwd=5, main="Selectivity at age")
})


## spawning biomass at age
output$SpawnBioAtAge <- renderPlot(
{
  ages <- c(1:input$amax)
  SB <- get_SB(ages, input$amat, input$Linf, input$k, input$t0, input$d, input$b, input$aselex, input$surv, input$u, 1)
  plot(ages, SB, col="tomato3", xlim=c(0, input$amax),
       ylim=c(0, max(SB)*1.1), type="l", lwd=5, main="Spawning Biomass at age")
}
  )

## catch at age
output$CatchAtAge <- renderPlot(
  {
    ages <- c(1:input$amax)
    Cb_a <- get_catch(ages, input$aselex, input$surv, input$u, 1, input$Linf, input$k, input$t0, input$d, input$b)
    plot(ages, Cb_a, col="darkred", xlim=c(0, input$amax), 
         ylim=c(0, max(Cb_a)*1.1), type="l", lwd=5, main="Catch at age")
    
  })

## spawning biomass per recruit in fished condition
output$SBPRf <- renderText(
  {
    get_SBPR(input$amat, input$Linf, input$k, input$t0, input$d, input$b, input$aselex, input$surv, input$u, 1, unfished=FALSE)
  })


## spawning biomass per recruit in unfished condition
output$SBPR0 <- renderText(
  {
    get_SBPR(input$amat, input$Linf, input$k, input$t0, input$d, input$b, input$aselex, input$surv, 0, 1, unfished=TRUE)
  })

## spawning potential ratio
output$SPR <- renderText(
  {
    get_SPR(input$amat, input$Linf, input$k, input$t0, input$d, input$b, input$aselex, input$surv, input$u, 1)
  })


## yield per recruit
output$YPR <- renderText(
  {
    get_YPR(input$aselex, input$surv, input$u, input$Linf, input$k, input$t0, input$d, input$b, 1)
  }
)


## end
}
)

