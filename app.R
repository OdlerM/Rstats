# Load R packages
library(shiny)
library(shinythemes)


# Define UI
ui <- fluidPage(theme = shinytheme("united"), 
                navbarPage(
                  'Central Limit Theorem',
                tabPanel('Underlying theory', 
                         includeMarkdown("clttheory.md")
                         ),
                tabPanel('Simulation study',
                  mainPanel(
                    sliderInput(inputId = 'noobs',
                                label = 'Number of observations:',
                                min = 2, max = 40, value = 2, step = 1),
                    checkboxInput(inputId = 'dens', 
                                  label = 'Estimated normal distribution density', 
                                  value = FALSE, width = NULL),
                    actionButton('submitbutton', 'Generate'),
                    plotOutput(outputId = 'plotik')
                  )
                ),
                tabPanel('TBA', 'Blank')
                )
)

# Define server function  
server <- function(input, output) {
  x <- eventReactive(input$submitbutton, {
    apply(sapply(1:5000, function(x) {runif(input$noobs)}), 2, mean)
    })

  output$plotik <- renderPlot({
    xseq <- seq(min(x()), max(x()), length = 2000)
    yseq <- dnorm(xseq, mean = mean(x()), sd = sd(x()))
    hist(x(), xlab = '', prob = TRUE, ylab = 'Prob',
         main = '', bty = 'o', breaks = 20, 
         ylim = c(0, max(yseq) + 0.2), col = 'lightblue')
    if (input$dens) {
      lines(xseq, yseq, col = 'red', lwd = 1.5)
    }
  })
} 

# Create Shiny object
shinyApp(ui = ui, server = server)