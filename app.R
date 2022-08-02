# Load R packages
library(shiny)
library(shinythemes)

# Define UI
ui <- fluidPage(theme = shinytheme("united"), 
                navbarPage(
                  'Central Limit Theorem',
                tabPanel('Underlying theory', 
                         includeHTML("clttheory.html")
                         ),
                
                tabPanel('Simulation study, continuous distributions',
                  mainPanel(
                    includeMarkdown('cont.md'),
                    selectInput("distr", "Select distribution:",
                                c("Uniform" = "uni",
                                  "Exponential" = "exp",
                                  "Chi-squared" = "chisq")),
                    sliderInput(inputId = 'noobs',
                                label = 'Number of observations:',
                                min = 2, max = 40, value = 2, step = 1),
                    checkboxInput(inputId = 'dens', 
                                  label = 'Estimated normal distribution density', 
                                  value = FALSE, width = NULL),
                    checkboxInput(inputId = 'theor', 
                                  label = 'Theoretical normal distribution density', 
                                  value = FALSE, width = NULL),
                    actionButton('submitbutton', 'Generate'),
                    plotOutput(outputId = 'plotik')
                  )
                ),
                
                tabPanel('Simulation study, discrete distributions',
                         mainPanel(
                           includeMarkdown('dist.md'),
                           selectInput("distr2", "Select distribution:",
                                       c("Bernoulli" = "bern",
                                         "Poisson" = "pois")),
                           sliderInput(inputId = 'noobs2',
                                       label = 'Number of observations:',
                                       min = 2, max = 60, value = 2, step = 2),
                           checkboxInput(inputId = 'dens2', 
                                         label = 'Estimated normal distribution density', 
                                         value = FALSE, width = NULL),
                           checkboxInput(inputId = 'theor2', 
                                         label = 'Theoretical normal distribution density', 
                                         value = FALSE, width = NULL),
                           actionButton('submit2', 'Generate'),
                           plotOutput(outputId = 'plotik2')
                         )
                ))
)

# Define server function 
server <- function(input, output) {
  ###  Continuous distributions
  x <- eventReactive(input$submitbutton, {
    if (input$distr == 'uni') {
    apply(sapply(1:5000, function(x) {runif(input$noobs)}), 2, mean) }
    else if (input$distr == 'exp') {
      apply(sapply(1:5000, function(x) {rexp(input$noobs, rate = 4)}), 2, mean)
    }
    else {
      apply(sapply(1:5000, function(x) {rchisq(input$noobs, df = 3)}), 2, mean)
    }
    })
  
  # Parameters of chosen distributions 
  mu <- eventReactive(input$submitbutton, {
    if (input$distr == 'uni') {1/2}
    else if (input$distr == 'exp') {1/4}
    else {3}
  })
  stddev <- eventReactive(input$submitbutton, {
    if (input$distr == 'uni') {sqrt(1 / 12 / input$noobs)}
    else if (input$distr == 'exp') {sqrt(1 / 16 / input$noobs)}
    else {sqrt(6 / input$noobs)}
  })
  
  # Xlabel 
  expr <- eventReactive(input$submitbutton, {
    if (input$distr == 'uni') 'X ~ Unif[0, 1]'
    else if (input$distr == 'exp') 'X ~ Exp(4)'
    else expression(paste('X ~ ', chi, '(3)', sep = ''))
  })
  poc <- eventReactive(input$submitbutton, {
    input$noobs
  })
  
  # Graph 1  
  output$plotik <- renderPlot({
    # Set values for estimated normal distributions
    xseq <- seq(min(x()), max(x()), length = 2000)
    yseq <- dnorm(xseq, mean = mean(x()), sd = sd(x()))
    topper <- max(hist(x())$density)
    
    # Draw histogram
    hist(x(), xlab = '', prob = TRUE, ylab = 'Prob',
         main = '', breaks = 20, 
         ylim = c(0, max(yseq, topper) + 0.2), col = 'lightblue')
    mtext(expr(), side = 1, line = 2.5)
    mtext(paste('n = ', poc(), sep = ''), side = 1, line = 3.6)
    
    if (input$dens) {
      lines(xseq, yseq, col = 'red', lwd = 1.5)
    }
    if (input$theor)
      lines(xseq, dnorm(xseq, mean = mu(), sd = stddev()), col = 'orange', 
            lwd = 1.5) 
    })
  
  ### Discrete distributions
  y <- eventReactive(input$submit2, {
    if (input$distr2 == 'bern') {
      apply(sapply(1:5000, 
                   function(x) {sample(0:1, input$noobs2, replace = T)}), 2, mean) 
      }
    else  {
      apply(sapply(1:5000, function(x) {rpois(input$noobs2, 3)}), 2, mean)
    }
    
  })
  
  # Parameters of chosen distributions 
  mu2 <- eventReactive(input$submit2, {
    if (input$distr2 == 'bern') {1 / 2}
    else  {3}
  })
  
  stddev2 <- eventReactive(input$submit2, {
    if (input$distr2 == 'bern') {1 / input$noobs2 / 4}
    else {3 / input$noobs2}
  })
  
  # Xlabel
  expr2 <- eventReactive(input$submit2, {
    if (input$distr2 == 'bern') 'X ~ Bern(0.5)'
    else 'X ~ Po(3)'
  })
  poc2 <- eventReactive(input$submit2, {
    input$noobs2
  })
  
  # Graph 2
  output$plotik2 <- renderPlot({
    # Set values for estimated normal distributions
    xseq2 <- seq(min(y()) - 0.2, max(y()) + 0.2, length = 2000)
    yseq2 <- pnorm(xseq2, mean = mean(y()), sd = sd(y()))
    
    # Draw ECDF
    plot(ecdf(y()), main = '', xlab = '', 
         ylab = 'Empirical distributive function')
    mtext(expr2(), line = 2.5, side = 1)
    mtext(paste('n = ', poc2(), sep = ''),  side = 1, line = 3.6)
    
    if (input$dens2) {
      lines(xseq2, yseq2, col = 'red', lwd = 1.5)
    }
    if (input$theor2)
      lines(xseq2, pnorm(xseq2, mean = mu2(), sd = sqrt(stddev2())), 
            col = 'orange', lwd = 1.5) 
  })
} 

# Create Shiny object
shinyApp(ui = ui, server = server)