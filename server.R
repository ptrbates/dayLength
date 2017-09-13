library(shiny)
library(ggplot2)
if(!exists('df.dayLength')) {
  load('dayLength/dayLength.Rdata')
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$lengthPlot <- renderPlot({
    ind <- which(df.dayLength$date == as.Date(input$date, 
                                              format = "%a %b %d %H:%M:%S %Y"))
    # generate a ggplot line graph of the length of each day through the year
    ggplot(data = df.dayLength, aes(x = date, y = lng / 3600)) +
      geom_line() +
      geom_point(aes(x = df.dayLength[ind,]$date, 
                     y = df.dayLength[ind,]$lng / 3600),
                 color = "dark red",
                 size = 2) +
      labs(title = "Length of Days, 2017",
           y = "Length of Day (hrs)",
           x = "Date")
  })
  output$text1 <- renderUI({
    ind <- which(df.dayLength$date == as.Date(input$date, 
                                              format = "%a %b %d %H:%M:%S %Y"))
    leng <- df.dayLength[ind,]$lng
    HTML(paste("Length of day selected: <br/>", 
               round(leng / 3600, 1),
               " hours (",
               leng,
               " seconds)",
               sep = ""))
  })
  output$text2 <- renderUI({
    ind <- which(df.dayLength$date == as.Date(input$date, 
                                              format = "%a %b %d %H:%M:%S %Y"))
    chng <- df.dayLength[ind,]$chg
    HTML(paste("Daily change in daylight: <br/>", 
               round(chng / 3600, 3),
               " hours (",
               chng,
               " seconds)", sep = ""))
  })
  
})
