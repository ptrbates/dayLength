library(jsonlite)
library(dplyr)
library(purrr)

if(!exists('df.dayLength')) {
  
  if(file.exists('dayLength.Rdata')) {
    load('dayLength.Rdata')
    
  } else {
    
    # Construct the sequence of dates we are intereted in
    daterange <- c("2017-01-01", "2017-12-31")
    dates <- seq.Date(as.Date(daterange[1]), 
                      as.Date(daterange[2]),
                      "days")
    
    # Record the latitude and longitude of interest
    latlong <- c(45.44317, -122.639132)
    
    # Construct the first part of the request
    request <- paste('https://api.sunrise-sunset.org/json?formatted=0&lat=',
                     latlong[1], 
                     '&lng=', 
                     latlong[2], 
                     '&date=', sep = '')
    
    # Append the dates to the first part of the request
    requests <- map2_chr(request, dates, paste, sep = '')
    
    # Make the requests from the API and save the results as day_lengths
    # Sunrise and sunset times obtained from https://sunrise-sunset.org/api
    day_lengths <- requests %>%
      map(fromJSON) %>%
      map(1) %>%
      map(4) %>%
      unlist()
    
    df.dayLength <- data.frame(date = dates, day_length = day_lengths) %>%
      mutate(change = c(NA, diff(day_length)), 
             normal_day_length = ((day_length - mean(day_length)) / sd(day_length)),
             normal_change = ((change - mean(change, na.rm = T)) / sd(change, na.rm = T)))
    
    # Save the resulting data frame to an .Rdata file
    save(df.dayLength, file = "dayLength.Rdata")
    
  }
}