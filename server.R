library(shiny)
library(ggplot2)

# Load the data frame
load('dayLength.Rdata')

shinyServer(function(input, output) {
  
  # Define plot1
  output$plot1 <- renderPlot({
    # Find the index of the date selected
    ind <- which(df.dayLength$date == as.Date(input$date, 
                                              format = "%a %b %d %H:%M:%S %Y"))
    
    # Specify the plot labels, given the radio option selected
    titles <- switch(input$radio1,
                     day_length = c("Daylight Curve", 
                                    "", "Day Length (hr)"),
                     change = c("Change in Daylight", 
                                "", "Change in Day Length (s)"),
                     total = c("Total Light", 
                               "", "Total Light (hr)"))
    
    # Specify the limits for each graph, given the radio option selected
    lims <- switch(input$radio1,
                   day_length = c(0, 60000 / 3600),
                   change = c(-300, 300),
                   total = c(0, 60000 / 3600))
    
    # Find the length of the day at the index selected
    leng <- df.dayLength[ind,]$day_length
    
    # Find the change in daylight for the index selected
    chng <- df.dayLength[ind,]$change
    
    # Find the total light received until the index selected
    trec <- sum(df.dayLength$day_length[1:ind])
    
    # Display a message, depending on which radio button option is selected
    annotation <- switch(input$radio1,
                         day_length = HTML(paste("Total daylight on ",
                                                 format(as.Date(df.dayLength$date[ind]),
                                                        "%B %d"),
                                                 ": \n", 
                                                 round(leng / 3600, 2),
                                                 " hours",
                                                 sep = "")),
                         change = HTML(paste("Change in daylight for ",
                                             format(as.Date(df.dayLength$date[ind]),
                                                    "%B %d"),
                                             ": \n", 
                                             chng,
                                             " seconds", sep = "")),
                         total = HTML(paste("Total light received this year: \n", 
                                            round(trec / 3600, 1),
                                            " hours (",
                                            round(100 * trec / sum(df.dayLength$day_length), 1),
                                            "% of total for the year)", sep = ""))
    )
    
    # Generate a ggplot line graph of the year, by the radio button option selected
    g <- ggplot(data = df.dayLength, aes(x = date))
    if(input$radio1 == "total") {
      g <- g + geom_area(aes(x = date, 
                             y = c(df.dayLength$day_length[1:ind], 
                                   rep(0, length(df.dayLength$day_length) - ind)) / 3600),
                         fill = "yellow") +
        geom_line(aes(y = df.dayLength$day_length / 3600))
    } else if(input$radio1 == "change") {
      g <- g + geom_line(aes(y = df.dayLength$change)) +
        geom_point(aes(x = df.dayLength[ind,]$date, 
                       y = df.dayLength[ind,]$change),
                   color = "dark red",
                   size = 2.5)
    } else {
      g <- g + geom_line(aes(y = df.dayLength$day_length / 3600)) +
        geom_point(aes(x = df.dayLength[ind,]$date, 
                       y = df.dayLength[ind,]$day_length / 3600),
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
      theme(axis.title = element_text(size = 17)) +
      theme(axis.text.x = element_text(size = 12)) +
      annotate("text", 
               x = as.Date('2017-01-07'), 
               y = lims[1] + lims[2] / 12, 
               label = annotation,
               hjust = 0,
               size = 4)
    
  })
  
  output$text1 <- renderUI({
    switch(input$radio1,
           day_length = HTML("In the summer, we receive more hours of daylight. In the winter, the hours of daylight we receive are reduced. The curve on this graph represents the relationship between the day of the year and the length of daylight on that day, in Portland, Oregon."),
           change = HTML("Since each day is longer or shorter than the next, we can conclude that each day we gain or lose a certain amount of daylight. The curve on this graph represents the amount of daylight gained or lost each day.</br></br>It also shows how quickly the 'Daylight Curve' is changing: sometimes it changes quickly, other times more slowly. When the 'Daylight Curve' changes quickly, the 'Change in Daylight' curve is near a peak. When the 'Daylight Curve' changes slowly, the 'Change in Daylight' curve is near zero.</br></br>The 'Change in Daylight' curve is the <i>derivative</i> of the 'Daylight Curve'. This is the idea of the derivative: it describes <i>how a function changes</i>."),
           total = HTML("Starting with a derivative and reconstructing the original function is the goal of <i>integration</i>.</br></br>On this graph we can see the result of applying integration to the original 'Daylight Curve'. In this case, since the curve was a representation of how much light was received per day, the integral represents the total amount of light received between January 1 and the date in question.")
    )
  })
  
  output$text2 <- renderUI({
    switch(input$radio1,
           day_length = HTML("</br>Exercises:</br></br>What general class of function might be useful for describing this curve?</br></br>Write a specific equation that approximates this curve."),
           change = HTML("</br>Exercises:</br></br>Can you imagine going backwards from the derivative to the function itself? That is, if I told you where to start and gave you the 'Change in Daylight' curve, could you reconstruct the 'Daylight Curve'?</br></br>January 1 had 8.81 hours (31714 seconds) of daylight: give it a try!"),
           total = HTML("</br>The area under the 'Daylight Curve' is the <i>integral</i> of the 'Daylight Curve'. The integral is the inverse of the derivative; we can think of the integral as the summation of a function between given points."))
  })
  
})
