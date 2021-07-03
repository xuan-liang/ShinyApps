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
  
  # Application title
  titlePanel("Old Faithful Geyser Data (faithful)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "n_breaks",
                  label = "Number of bins:",
                  value=20,min=1,max=200,step=1),
      checkboxInput(inputId = "prob",
                    label = strong("Show normalised histogram plot"),
                    value = FALSE), 
      checkboxInput(inputId = "density",
                    label = strong("Show smoothed density estimate"),
                    value = FALSE),
      conditionalPanel(condition = "input.density == true && input.prob == true",
                       sliderInput(inputId = "bw_adjust",
                                   label = "Bandwidth adjustment:",
                                   min = 0.1, max = 2, value = 1, step = 0.1)),
      br(), 
      br(),
      helpText("This Shiny App is developed by Dr. Xuan Liang.",style="font-size:80%;")
      
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    if(input$prob){
      hist(faithful$eruptions,
           probability = TRUE,
           breaks = seq(min(faithful$eruptions),max(faithful$eruptions),length.out=as.numeric(input$n_breaks)+1),
           xlab = "Duration (minutes)",
           main = "Density of Geyser Eruption Duration",ylab="Density")
    }else{
      hist(faithful$eruptions,
           breaks = seq(min(faithful$eruptions),max(faithful$eruptions),l=as.numeric(input$n_breaks)+1),
           xlab = "Duration (minutes)",
           main = "Histogram of Geyser Eruption Duration",ylab="Count")
    }
    
    
    if (input$density&input$prob) {
      dens <- density(faithful$eruptions,
                      adjust = input$bw_adjust)
      lines(dens, col = "blue")
    }
    if (input$density&input$prob) {
      dens <- density(faithful$eruptions,
                      adjust = input$bw_adjust)
      lines(dens, col = "blue")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

