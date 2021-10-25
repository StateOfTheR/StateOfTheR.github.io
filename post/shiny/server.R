library(ggplot2)
library(MASS)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
## Initialisation steps ##
  # Parameters for the distribution
  nr_obs <- 100
  mu1  <- 1
  mu2  <- 1
  sig1 <- 1
  sig2 <- 1

  # Generate random data
  d <- eventReactive(input$newPlot,{
    rho  <- runif(1,-1,1)
    df <- mvrnorm(n=nr_obs, mu=c(mu1,mu2), Sigma=matrix(c(sig1,rho,rho,sig2), 2))
    colnames(df) <- c('x','y') 
    data.frame(df)  
    })
  
  # Scatterplot based on the normal distribution parameters
  output$scatterplot <- renderPlot({
    # Plot the data
   ggplot(d(),aes(x,y)) + geom_point()
  })
  
  # Action button : "Check guess"
  observeEvent(input$results, {
    output$res_guess <- renderTable({
      rValue <- cor(d()$x, d()$y)
      data.frame('rho' =  rValue, 'Guess' = input$rho)
    })
  })

  # Hide results when a new plot is generated
  observeEvent(input$newPlot,{
    output$corResults <- renderUI({
      checkboxInput("results","Show results",FALSE)
    })
  })
     
})

