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
  div(titlePanel("Dice Rollings"),style="color: #337ab7;font-size:150%;"), 
  
  # tags$head(
  #   tags$style(
  #     ".title {margin: auto; width: 200px}"
  #   )
  # ),
  # tags$div(class="title", titlePanel("Dice Rollings")),
  # tags$style(type='text/css', "select {font-size: 32px !important} "),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("n",
                  "Number of times to roll the dice:",
                  min = 1,
                  max = 5,
                  value = 1),
      br(), 
      
      actionButton("button", "Click the button to roll the dice", 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4;padding:4px;"),
      br(), 
      br(), 
      helpText("This Shiny App is developed by Dr. Xuan Liang.",style="font-size:80%;")
      
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      div(p(strong("Table of Outcome")),style="font-size:120%;margin-left:25px;"),
      br(),
      div(tableOutput("table"), align = "center",style="font-size:120%;"),
      br(),
      div(textOutput("summary"), align = "center",style="color:red;font-size:150%;")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  TotalID=c("1st time", "2nd time", "3rd time","4th time","5th time")
  
  a<- reactive(
    {
      if (is.null(input$n)){
      shiny:::flushReact()
      return()
    }else{
    d=rep("",input$n)
    if(input$button)
    {
    d=sample(1:6,input$n,replace=T)
    }
    return(d)
    }
  }
  )
  
  output$table <- renderTable(
    {  
      if (is.null(input$n)){
        shiny:::flushReact()
        return()
      }else {
        data.frame("Rolling"=TotalID[1:input$n], "Outcome"=a()) 
      }
    }
  )
  output$summary <-renderText(
    {
      if (is.null(input$n)){
        shiny:::flushReact()
        return()
      }else if(input$button){
      nsix=sum(a()==6)
      text=paste("The number of times to get 6 is ",nsix ,".")
      }else{
        text=paste("How many times could we get a six?")
      }
      return(text)
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)

