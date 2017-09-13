library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Length of Days (hours)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      htmlOutput("text1"),
      br(),
      htmlOutput("text2"),
      br(),
      dateInput("date", 
                "Please select the date of interest:",
                min = min(df.dayLength$date),
                max = max(df.dayLength$date),
                value = date())
    ),
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("lengthPlot")
    )
  )
))
