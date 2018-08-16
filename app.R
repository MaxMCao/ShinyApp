#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    pageWithSidebar(
        headerPanel("Max playing shiny in Rstudio"),
        sidebarPanel(
            selectInput('Distribution',
                        'Please select distribution type',
                        choices = c('Normal', 'Exponential')),
            sliderInput("sampleSize",
                        'Please select sample size: ',
                        min = 100,
                        max = 5000,
                        value = 1000,
                        step = 100),
            conditionalPanel(condition = "input.Distribution == 'Normal'",
                             textInput('Mean', 'Please select the mean',
                                       value = 10),
                             textInput('sd', 'Please select standard deviation',
                                       value = 3)
                             ),
            conditionalPanel(condition = "input.Distribution == 'Exponential'",
                             textInput('lambda', 'Please select exponent lambda: ',
                                       value = 1))
        ),
        mainPanel(
            plotOutput("myPlot")
        )
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$myPlot <- renderPlot({
        distType <- input$Distribution
        size <- input$sampleSize
        if(distType == "Normal"){
            randomvec <- rnorm(size, mean = as.numeric(input$Mean), sd = as.numeric(input$sd))
        }
        else{
            randomvec <- rexp(size, rate = 1/as.numeric(input$lambda))
        }
        hist(randomvec, col = 'blue')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

