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
 
  headerPanel("One Sample Test for Normal Distribution \\(N(\\mu,\\sigma^2)\\) when \\(\\sigma^2\\) is known"),
  helpText("",style="text-indent:15px;font-size:125%;"),
  
  sidebarLayout(
    
    sidebarPanel(  
      
      withMathJax(),
      
      h3("Hypotheses", style="color: #337ab7"),
      
      br(),
      
      strong("\\( H_0:\\mu = \\mu_0\\) vs \\(H_1: \\mu\\neq \\mu_0\\)"),
      
      br(),
      
      br(),
      
      sliderInput("mu0",
                   label = "Value of \\( \\mu_0\\)",
                   min = 45, max= 75, value = 60, step=0.5
      ),
      
      
      br(),
      
      
      
      h3("Sample", style="color: #337ab7"),
      
      br(),
      
      radioButtons("type", "Sample is generated given",
                   c("\\(H_0\\) is true" = "1",
                     "\\(H_0\\) is false" = "2")
      ),
        
      br(),
      
      sliderInput("n",
                  label = "Sample size \\(n\\)",
                  min = 5, max= 100, value = 10
      ),
      
      uiOutput("pmean"),
      
      sliderInput("sigma2",
                  label = "Variance \\(\\sigma^2\\)",
                  min = 25, max= 50, value = 36
      ),
      
      
      actionButton("button", "Click to generate a new sample", icon = icon("table"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4;padding:4px;"),
      
      br(),
      
      br(),
      
      br(),
      
      hr(),
      
      helpText("This Shiny App is developed by Dr. Xuan Liang.",style="font-size:80%;"),
      
      width = 3
    ),
    
    mainPanel(
      h4("Sample", style="color: #337ab7"),
      checkboxInput("checkdata",label="Show the sample in a table.",value=FALSE),
      textOutput("summary"),
      br(),
      tableOutput("table"),
      
      h4("Plot of rejection region \\(|\\bar X-\\mu_0|>c\\) (shown in red)", style="color: #337ab7"),
      
      column(8, div(plotOutput("plot"), align = "center")),
      column(4, 
             
             br(),
             br(),
             
           
             
             # div(textOutput("RR")),
             strong("Rejection Region:"),
             
             div(uiOutput("RR"), align="left"),
             
             br(),
             div(sliderInput("c",label = "Value of c",min = 1, max= 5, value = 2, step=0.1), align = "left"),
             
             br(),
             
             div(uiOutput("ptitle"), align="left"),
             br(),
             
             div(uiOutput("p"), align="left")
             ),
      
      br(),
      
      column(6,
             uiOutput("qresult"),
             br(),
             uiOutput("ans1"),
             uiOutput("printans1")
             ),
      
      column(6,uiOutput("qerror"),
             br(),
             uiOutput("ans2"),
             uiOutput("printans2")
      ),
    )
  )
)


server <- function(input, output) {
  
  output$pmean = renderUI(
      if (input$type=="2"){
        withMathJax(sliderInput("mu",
                    label = "Population mean \\(\\mu\\) (not equal to \\(\\mu_0\\))",
                    min = 55, max= 65, value = 58, step=0.5))
      }
  )
  
  
  # output$RR <-renderText({ 
  #     upper <- input$mu0+input$c
  #     lower <- input$mu0-input$c
  #     text <- paste0("Rejection region: \\(\\bar X\\)>", upper," or ", "\\(\\bar X\\)<", lower)
  # })
  # 
  output$RR <- renderUI({
    upper <- input$mu0+input$c
    lower <- input$mu0-input$c
    withMathJax(sprintf("\\(\\bar X>\\) %.01f or \\(\\bar X<\\) %.01f", upper, lower))
  })
  
  d<- reactive({
      if(input$button&input$type=='1'){
        data <- rnorm(input$n, mean=input$mu0, sd=sqrt(input$sigma2))
      }else if(input$button&input$type=='2'){
            data <- rnorm(input$n, mean=input$mu, sd=sqrt(input$sigma2))
      }
    return(data)
})
  
  
  
  output$summary <-renderText({ 
    if (input$button){
      x <- d()
      n <- length(x)
      mean <- round(mean(x),1)
      sd <- round(var(x),1)
      text <- paste0("Size: ", n,";"," Sample mean: ", mean)
      return(text)
    }else{
      text <- paste0("Sample is not generated.")
      return(text)
    }  
    })
  
  output$table <-renderTable({ 
    if (input$button){
        if(input$checkdata)
        {
          xx <- round(d(),1)
          if (length(xx)%%10==0){
            xx <- c(xx, rep("", 0))
            matrix <- matrix(xx, ncol=10, byrow=TRUE)
          }else{
            xx <- c(xx, rep("", 10-length(xx)%%10))
            matrix <- matrix(xx, ncol=10, byrow=TRUE)
          }
          colnames(matrix) <- c(1:10)
          matrix
        }else{
          shiny:::flushReact()
          return()
        }
    }
    })
  
  output$plot <-renderPlot({ 
    if (input$button){
      xbar <- mean(d())
      mu0 <- input$mu0
      sigma2 <- input$sigma2
      n <- input$n
      c <-input$c
      
      if(input$type=="1")
      {
        mu <- mu0
        px <- seq(mu-1.5*sqrt(sigma2), mu+1.5*sqrt(sigma2), by = 0.1)
        y <- dnorm(px, mu, sqrt(sigma2/n))
      }else{
        mu <- input$mu
        px <- seq(mu-1.5*sqrt(sigma2), mu+1.5*sqrt(sigma2), by = 0.1)
        y <- dnorm(px, mu, sqrt(sigma2/n))
      }
        plot(px, y,type="l", xaxt = "n", xlab=expression(bar(X)),ylab="Density", main=expression(paste("Distribution of ",bar(X))))
        abline(v=mu, lty=2, col='grey')
        polygon(c(px[px>=mu0+c], rep(mu0+c, length(px>=mu0+c))), c(y[px>=mu0+c], rep(0, length(px>=mu0+c))), col="red",border = FALSE)
        polygon(c(px[px<=mu0-c], rep(mu0-c, length(px<=mu0-c))), c(y[px<=mu0-c], rep(0, length(px<=mu0-c))), col="red",border = FALSE)
        abline(v=xbar, lty=2, col="blue")
        axis(1, at=c(xbar, mu0-c, mu, mu0+c), labels = c(round(xbar,1),mu0-c, paste0("\u03BC", "=", mu), mu0+c))
        text(xbar, max(y)*0.9, "Observed sample mean", col="blue")
      
    }else{
      
      mu0 <- input$mu0
      sigma2 <- input$sigma2
      n <- input$n
      c <- input$c
      
      if(input$type=="1"){
        mu <- mu0
        px <- seq(mu0-1.5*sqrt(sigma2), mu0+1.5*sqrt(sigma2), by = 0.1)
        y <- dnorm(px, mu0, sqrt(sigma2/n))
      }else if(input$type=="2"){
        mu <- input$mu
        px <- seq(mu-1.5*sqrt(sigma2), mu+1.5*sqrt(sigma2), by = 0.1)
        y <- dnorm(px, mu, sqrt(sigma2/n))
      }
        plot(px, y,type="l", xaxt = "n", xlab=expression(bar(X)),ylab="Density", main=expression(paste("Distribution of ",bar(X))))
        abline(v=mu, lty=2, col='grey')
        polygon(c(px[px>=mu0+c], rep(mu0+c, length(px>=mu0+c))), c(y[px>=mu0+c], rep(0, length(px>=mu0+c))), col="red",border = FALSE)
        polygon(c(px[px<=mu0-c], rep(mu0-c, length(px<=mu0-c))), c(y[px<=mu0-c], rep(0, length(px<=mu0-c))), col="red",border = FALSE)
        axis(1, at=c(mu0-c, mu, mu0+c), labels = c(mu0-c, paste0("\u03BC", "=", mu), mu0+c))
    }  
  })
  
  output$qresult <- renderUI(
    if(input$button){
    radioButtons("testresult", "What is your decision?",
                 c("Reject the null hypothesis." = "yes",
                   "Fail to reject the null hypothesis." = "no"),selected="0")
    }
  )
  
  output$ans1 <- renderUI(
    if(input$button){
      checkboxInput("ans1",label="Show the correct answer.",value=FALSE)
    }
  )
  
  output$pans1 <-renderText({ 
    if (input$ans1){
      xbar <- mean(d())
      upper <- input$mu0+input$c
      lower <- input$mu0-input$c
      if(xbar > upper|xbar < lower){
        text <- "Answer: Reject the null hypothesis."
      }else{
        text <- "Answer: Fail to reject the null hypothesis."
      }
      return(text)
    }else{
      text <- paste0("")
      return(text)
    }  
  })
  
  output$printans1 <-renderUI({ 
    if (input$button){
      textOutput("pans1")
    }  
  })
  
  output$qerror<- renderUI(
    if(input$button){
      radioButtons("errortype", "Choose the correct statement regarding your decision.",
                   c("Correct decision!" = "correct",
                     "Type I error occurs." = "type1",
                     "Type II error occurs." = "tyep2"),
                   selected="0")
    }
  )
  
  output$ans2 <- renderUI(
    if(input$button){
      checkboxInput("ans2",label="Show the correct answer.",value=FALSE)
    }
    )
  
  output$pans2 <-renderText({ 
    if (input$ans2){
      xbar <- mean(d())
      upper <- input$mu0+input$c
      lower <- input$mu0-input$c
      type <- input$type
      if((xbar > upper|xbar < lower) & type=="1"){
        text <- "Answer: Type I error occurs."
      }else if(xbar < upper&xbar > lower & type=="2"){
        text <- "Answer: Type II error occurs."
      }else{
        text <- "Answer: Correct decision!"
      }
      return(text)
    }else{
      text <- paste0("")
      return(text)
    }  
  })
  
  output$printans2 <-renderUI({ 
    if (input$button){
      textOutput("pans2")
    }  
  })

  output$ptitle <- renderUI({
    if(input$type=="1"){
      strong("\\(\\alpha\\) = P(Type I error):")
    }else if(input$type=="2"){
      strong("\\(\\beta\\) = P(Type II error):")
    }
  })  
  
output$p <- renderUI({
    upper <- input$mu0+input$c
    lower <- input$mu0-input$c
    sigma2 <- input$sigma2
    n <- input$n
    if(input$type=="1"){
      mu0 <- input$mu0
      prob <- pnorm(lower, mu0 ,sqrt(sigma2/n)) + 1- pnorm(upper, mu0, sqrt(sigma2/n))
      withMathJax(sprintf("P(\\( \\bar X>\\) %.01f or \\(\\bar X<\\) %.01f) = %.04f", upper, lower, prob))
    }else if(input$type=="2"){
      mu <- input$mu
      prob <- pnorm(upper, input$mu ,sqrt(input$sigma2/input$n))- pnorm(lower, input$mu ,sqrt(input$sigma2/input$n))
      withMathJax(sprintf("P(\\(%.01f <\\bar X < %.01f\\)) = %.04f", lower, upper, prob))
    }
  })
}


# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
