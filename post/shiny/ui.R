library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Guess The Correlation"),
  
  sidebarPanel(
  tags$b("Game Instruction:"),tags$br(),tags$br(),
   "Step 1: Click on 'Get new plot'",tags$br(),tags$br(),
   "Step 2: Check the scatter plot on the right;",tags$br(),tags$br(),
   "Step 3: Slide the bar so it matches your best guess for the Pearson correlation coefficient between X and Y;",tags$br(), tags$br(),
   "Step 4: Check your guess by clicking 'Show results'",tags$br(),tags$br(),
   
   # To let the user estimate the correlation 
    sliderInput("rho", "Select correlation", min = -1, max = 1, value = 0.0, step=0.01),
   # To display the results conditionnaly to the checkbox
     uiOutput("corResults"),
   # To display the results
    conditionalPanel("input.results == true", tableOutput('res_guess')),
   # To create a new plot
    actionButton("newPlot", "Get new plot")
  ),
  
  mainPanel(
    plotOutput("scatterplot",width = "700px", height="500px")
    )
  
))