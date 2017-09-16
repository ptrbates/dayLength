library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$plot1 <- renderPlot({
    # Find the index of the date selected
    ind <- which(df.dayLength$date == as.Date(input$date, 
                                              format = "%a %b %d %H:%M:%S %Y"))
    
    # Specify the plot labels, given the radio option selected
    titles <- switch(input$radio1,
                     day_length = c("Total Day Length", 
                                    "Date", "Day Length (s)"),
                     change = c("Change in Day Length", 
                                "Date", "Change in Day Length (s)"),
                     total = c("Total Light Received This Year", 
                               "Date", "Total Light (s)"))
    
    # Specify the limits for each graph, given the radio option selected
    lims <- switch(input$radio1,
                   day_length = c(0, 60000),
                   change = c(-300, 300),
                   total = c(0, 60000))
    
    # Generate a ggplot line graph of the year, by the radio button option selected
    g <- ggplot(data = df.dayLength, aes(x = date))
    if(input$radio1 == "total") {
      g <- g + geom_area(aes(x = date, 
                             y = c(df.dayLength$day_length[1:ind], 
                                   rep(0, length(df.dayLength$day_length) - ind))),
                         fill = "yellow") +
        geom_line(aes(y = df.dayLength$day_length))
    } else {
      g <- g + geom_line(aes(y = df.dayLength[[input$radio1]])) +
        geom_point(aes(x = df.dayLength[ind,]$date, 
                       y = df.dayLength[ind,][[input$radio1]]),
                   color = "dark red",
                   size = 2.5)
    }
    g +
      geom_hline(yintercept = 0) + 
      geom_vline(xintercept = as.Date("2017-01-01")) +
      labs(title = titles[1],
           x = titles[2],
           y = titles[3]) +
      coord_cartesian(ylim = lims) +
      theme_linedraw() +
      theme(plot.title = element_text(size = 25)) +
      theme(plot.title = element_text(hjust = .5)) +
      theme(axis.title = element_text(size = 17))
    
  })
  
  output$text1 <- renderUI({
    # Find the index of the date selected
    ind <- which(df.dayLength$date == as.Date(input$date, 
                                              format = "%a %b %d %H:%M:%S %Y"))
    
    # Find the length of the day at the index selected
    leng <- df.dayLength[ind,]$day_length
    
    # Find the change in daylight for the index selected
    chng <- df.dayLength[ind,]$change
    
    # Find the total light received until the index selected
    trec <- sum(df.dayLength$day_length[1:ind])
    
    # Display a message, depending on which radio button option is selected
    switch(input$radio1,
           day_length = HTML(paste("Length of day selected: <br/>", 
                                   round(leng / 3600, 1),
                                   " hours (",
                                   leng,
                                   " seconds)",
                                   sep = "")),
           change = HTML(paste("Daily change in daylight: <br/>", 
                               round(chng / 3600, 3),
                               " hours (",
                               chng,
                               " seconds)", sep = "")),
           total = HTML(paste("Total light received this year: <br/>", 
                              round(trec / 3600, 1),
                              " hours <br/>(",
                              round(100 * trec / sum(df.dayLength$day_length), 1),
                              "% of total for the year)", sep = ""))
           
    )
  })
})
