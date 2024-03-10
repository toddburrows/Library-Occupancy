library("rvest")
library("lubridate")
library("ggplot2")
library("tidyverse")

CAPACITY <- 1800

getNewData <- function(Libdata){
  now()
  document <- read_html("https://apps.dur.ac.uk/study-spaces/library/bill-bryson/occupancy/display")
  free <- as.numeric(gsub(',', "", html_text(html_nodes(document, "text.donut-text")[1])))
  occupancy <- CAPACITY - free
  
  Libdata <- read.csv("/Users/toddb/Desktop/LibraryScrape/LibraryData")
  Libdata$Datetime <- as_datetime(Libdata$Datetime)
  newData <- data.frame("Datetime" = now(),
                    "Occupancy" = occupancy)
  print(newData)
  
  combinedData <- rbind(Libdata, newData)
  
  write.csv(combinedData, "/Users/toddb/Desktop/LibraryScrape/LibraryData", row.names = FALSE)
}


repeatData = function(interval = 30) {
  getNewData()
  Libdata <- read.csv("/Users/toddb/Desktop/LibraryScrape/LibraryData")
  Libdata$Datetime <- as_datetime(Libdata$Datetime)
  
  plot(Libdata)
  
  ggplot(read.csv("/Users/toddb/Desktop/LibraryScrape/LibraryData"), 
         aes(Datetime, Occupancy, group = 1)) +
    geom_line(col = "purple") +
    labs(x = "Date and Time", y = "Occupancy", 
         title = "The Occupancy of the Bill Bryson Library")
  
  later::later(repeatData, interval)
}


repeatData()

MLibdata <- read.csv("/Users/toddb/Desktop/LibraryScrape/LibraryData")
MLibdata$Datetime <- as_datetime(MLibdata$Datetime)
MLibdata |> 
  filter(Occupancy <1000) 
max(MLibdata$Occupancy)
