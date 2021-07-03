#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

ui <- pageWithSidebar(
  
  headerPanel(a(href="https://en.wikipedia.org/wiki/Binomial_distribution","Binomial Random Variable")),

  helpText("It is a discrete random variable
        that counts the number of successes in  
    n independent trials with probablity of having a success p for each trial.",style="text-indent:15px;font-size:125%;"),
  
# Input() functions

  sidebarLayout(
             sidebarPanel (  
        
               numericInput(inputId="n",
                            label='Number of trials (n)',
                            value=10),
               sliderInput(inputId="p",
                           label='Probability (p)',
                           value=0.5,min=0,max=1),
               
               br(),
             
               helpText("Model:"),
               div(textOutput("model"),style="text-indent:20px;font-size:125%;"),
               br(),
              selectInput(inputId = "tail",
                           label = "Probability:",
                           choices = c("Equality"="equal",
                                       "Lower Tail"="lower", 
                                       "Upper Tail"="upper", 
                                       "Middle"="middle",
                                       "Both Tails"="both"
                           ),
                           selected = "lower"),
               uiOutput("bounda"),
               uiOutput("boundb"),
               uiOutput("a"),
               uiOutput("b"),
               
               br(),
               checkboxInput("checktable",label="Create a table of all binomial probabilities",value=FALSE),
               
              br(), 
              br(),
              helpText("This Shiny App is developed by Dr. Xuan Liang.",style="font-size:80%;")
              
             ),
             
             mainPanel(
               plotOutput("plot"),
               div(textOutput("area"), align = "center", style="font-size:120%;"),
               br(),
               br(),
               div(tableOutput("tablep"), style="font-size:100%;")
             )
  
  )
)


server <- function(input, output) {

  
    output$tablep<- renderTable({
  
      if(input$checktable)
      {
        d = dbinom(0:input$n,input$n,input$p)
        data.frame("Sucesses"=0:input$n,"Probablities"=d)
      }
    },digits=4,align='cc')
    
    
    
    output$a = renderUI(
      {
        #print("a")
        value=1
        min=0
        max=1
        step=1
        
        if (is.null(input$n)){
          shiny:::flushReact()
          return()
        }
        
        value = round(input$n/4)
        min = 0
        max = input$n
        step = 1
        
        sliderInput("a", "a",
                    value = value,
                    min   = min,
                    max   = max,
                    step  = step)
      })
    
    output$b = renderUI(
      {
        #print("b")
        
        if (input$tail %in% c("equal","lower","upper")|is.null(input$n)){
          shiny:::flushReact()
          return()
        }
        
        if(input$tail %in% c("middle","both"))
        {
          value=1
          min=0
          max=1
          step=1
          
          value = round(input$n/4)
          min = 0
          max = input$n
          step = 1
          
          sliderInput("b", "b",
                      value = max(input$n-2,input$a+1),
                      min   = input$a,
                      max   = max,
                      step  = step)
        }
      })
    
    output$bounda = renderUI(
      {
          if (is.null(input$tail))
          {
            shiny:::flushReact()
            return()
          }
        
        if (input$tail!="equal") checkboxInput(inputId = "bounda",label = "Bound of a" ,value=FALSE)
          
      }
     )
    
    output$boundb = renderUI(
      {
        #print("upper bound")
      
          if (is.null(input$tail))
          {
            shiny:::flushReact()
            return()
          }
          
        if (input$tail=="middle"|input$tail=="both") checkboxInput(inputId = "boundb",label = "Bound of b" ,value=FALSE)
          
        }
      )
    
    get_model_text = reactive(
      {
        if (is.null(input$tail)){
          shiny:::flushReact()
          return()
        }
        
        low_less = "<"
        low_greater = ">"
        
        up_less = "<"
        up_greater = ">"
        
  
        if (input$tail!= "equal")
        {
          if (is.null(input$a))
          {
            shiny:::flushReact()
            return()
          }
          
          if (input$bounda)
          {
            low_less = "\u2264"
            low_greater = "\u2265"
          }
          
          if (input$tail %in% c("middle","both"))
          { 
            if (is.null(input$b)){
              shiny:::flushReact()
              return()
            }
            
            if (input$boundb)
            {
              up_less = "\u2264"
              up_greater = "\u2265"
            }
          }
        }
        
        text = ""
        if (length(input$tail) != 0)
        {
          if (input$tail == "equal")
          {
            # P(X = a)
            text = paste0("P(X = a)")
          }
          else if (input$tail == "lower")
          {
            # P(X < a)
            text = paste0("P(X ", low_less, " a)")
          }
          else if (input$tail == "upper")
          {
            # P(X > a)
            text = paste0("P(X ", low_greater, " a)")
          }
          else if (input$tail == "middle")
          {
            # P(a < X < b)
            text = paste0("P(a ", low_less, " X ", up_less, " b)")
          }
          else if (input$tail == "both")
          {
            # P(X < a or X > b)
            text = paste0("P(X ", low_less, " a or X ", up_greater, " b)")
          }
        }
        return(text)
      })
    
    output$model = renderText(
      {
        #print("model")
        
        get_model_text()
      })
    
    
  
        
    output$area = renderText(
      {
        if (is.null(input$tail) | is.null(input$a))
        {
          shiny:::flushReact()
          return()
        }
        
        L = input$a
        U = NULL
        
        if (input$tail %in% c("both","middle")) 
        {
          if (is.null(input$b))
          {
            shiny:::flushReact()
            return()
          }
          
          U = input$b
          
           error = FALSE
           if (L>U) error = TRUE
           if (error){
             return()
           }
        }
      
        
      f = function() NULL
        
      if (is.null(input$n) | is.null(input$p) | is.null(input$a))
          {
            shiny:::flushReact()
            return()
          }
          
          if (input$tail == "equal")
          {
            f = function(x) dbinom(x,input$n,input$p)
          }
          else
          {
            f = function(x) pbinom(x,input$n,input$p)
            
            if (input$tail %in% c("lower","both") & !input$bounda) L = L-1
            if (input$tail %in% c("upper")        & input$bounda) L = L-1
            if (input$tail %in% c("middle")       & input$bounda) L = L-1
            
            if (input$tail %in% c("both","middle")) 
            {
              if (is.null(input$boundb))
              {
                shiny:::flushReact()
                return()
              }
              
              if (input$tail == "both"   & input$boundb) U = U-1
              if (input$tail == "middle" & !input$boundb) U = U-1
            } 
          }
        
        val = NA
        if (input$tail == "lower")
          val = f(L)
        else if (input$tail == "upper")
          val = 1-f(L)
        else if (input$tail == "equal")
          val = f(L)
        else if (input$tail == "both")
          val = f(L) + (1-f(U))
        else if (input$tail == "middle")
          val = f(U) - f(L)
        
        text = paste(get_model_text(),"=",signif(val,3))
        
        text = sub("a",input$a,text)
        if (input$tail %in% c("both","middle")) 
        {
          text = sub("b",input$b,text)
          }
        
        text
      })
    
    output$plot = renderPlot({
      if (is.null(input$tail) | is.null(input$a))
      {
        shiny:::flushReact()
        return()
      }
      
      L = NULL
      U = NULL
      
      error = FALSE
      
      if (input$tail == "lower" | input$tail == "equal")
      {
        L = input$a 
      }
      else if (input$tail == "upper")
      {
        U = input$a 
      }
      else if (input$tail %in% c("both","middle"))
      {
        if (is.null(input$b)){
          shiny:::flushReact()
          return()
        }
        
        L = input$a
        U = input$b
        
        if (L > U)
          error = TRUE
      }
      
      if (error)
      {
        plot(0,0,type='n',axes=FALSE,xlab="",ylab="",mar=c(1,1,1,1))
        text(0,0,"Error: Lower bound greater than upper bound.",col="red",cex=2)
      }
      else
      {
        if(  is.null(input$n)
             | is.null(input$p)
             | is.null(input$a))
        {
          shiny:::flushReact()
          return()
        }
        
        if(input$tail %in% c("both","middle") & is.null(input$b))
        {
          shiny:::flushReact()
          return()
        }
        
        d = dbinom(0:input$n,input$n,input$p)
        
        plot(0,0,type='n',xlim=c(-0.5,input$n+0.5),ylim=c(0,max(d)),
             xlab="",ylab="", axes=FALSE)
        axis(1, cex.axis=1.5)
        axis(2, cex.axis=1.5)
        title(main=paste("Binomial Distribution"))
        
        for (k in 1:length(d)) 
        {
          col = NA
          
          if (input$tail == "lower")
          {
            if (!input$bounda   & k-1 <  L) col = "#569BBD"
            if (input$bounda  & k-1 <= L) col = "#569BBD"
          }
          else if (input$tail == "upper")
          {
            if (!input$bounda   & k-1 >  U) col = "#569BBD"
            if (input$bounda  & k-1 >= U) col = "#569BBD"
          }
          else if (input$tail == "equal")
          {
            if (k-1 == L) col = "#569BBD"
          }
          else if (input$tail == "both")
          {
            if (!input$bounda    & !input$boundb   & (k-1 <  L | k-1 >  U)) col = "#569BBD"
            if (!input$bounda  & input$boundb & (k-1 <  L | k-1 >= U)) col = "#569BBD"
            if (input$bounda  & !input$boundb   & (k-1 <= L | k-1 >  U)) col = "#569BBD"
            if (input$bounda  & input$boundb  & (k-1 <= L | k-1 >= U)) col = "#569BBD"
          }
          else if (input$tail == "middle")
          {
            if (!input$bounda   & !input$boundb   & k-1 >  L & k-1 <  U) col = "#569BBD"
            if (!input$bounda   & input$boundb & k-1 >  L & k-1 <= U) col = "#569BBD"
            if (input$bounda & !input$boundb   & k-1 >= L & k-1 <  U) col = "#569BBD"
            if (input$bounda  & input$boundb  & k-1 >= L & k-1 <= U) col = "#569BBD"
          }
          
          p = matrix(c(-1.5+k,0, -0.5+k,0, -0.5+k,d[k], -1.5+k,d[k], -1.5+k,0),ncol=2,byrow=TRUE)
          
          polygon(p, col=col)
        }
      }
      
    })
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
