library(shiny)

# Load the data frame
load('dayLength.Rdata')

shinyUI(fluidPage(
  
  # Application title
  titlePanel(HTML("<center>Introduction to the Derivative and the Integral</center></br>"), 
             windowTitle = "Introduction to Calculus with dayLength"),
  
  # Sidebar includes date selection, as well as view selection
  sidebarLayout(
    sidebarPanel(
      
      # Date selection
      dateInput("date", 
                "Select the date of interest:",
                min = min(df.dayLength$date),
                max = max(df.dayLength$date),
                value = date()),
      
      # View selection
      radioButtons("radio1", "Select the desired view:",
                   c("Length of Day" = "day_length",
                     "Change in Day Length" = "change",
                     "Total Light Time" = "total"),
                   selected = "day_length"),
      htmlOutput("text1")
      
    ),
    
    # Main panel shows the view selected
    mainPanel(
      plotOutput("plot1"),
      htmlOutput("text2")
    )
  )
))
