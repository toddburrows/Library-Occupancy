#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Written by Todd Burrows (Durham University)



#Load required packages
library("shiny")
library("rvest")
library("lubridate")
library("ggplot2")

#Set capacity of the library as a constant
CAPACITY <- 1800

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Bill Bryson Library Tracker"),
  mainPanel(
    plotOutput("tracker")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Function to scrape new data, and write the new data to a file
  getNewData <- function(Libdata){
    
    #Reading from webpage
    document <- read_html("https://apps.dur.ac.uk/study-spaces/library/bill-bryson/occupancy/display")
    
    #Retrieving the number of free seats then calculating occupancy
    free <- as.numeric(html_text(html_nodes(document, "text.donut-text")[1]))
    occupancy <- CAPACITY - free
    
    #Retrieving existing historic data
    Libdata <- read.csv("/Users/toddb/Desktop/LibraryScrape/LibraryData")
    Libdata$Datetime <- as_datetime(Libdata$Datetime)
    
    #Adding the new data to exisiting data
    newData <- data.frame("Datetime" = now(),
                          "Occupancy" = occupancy)
    combinedData <- rbind(Libdata, newData)
    
    #Writing the data to file, overwriting what is in the file
    write.csv(combinedData, "/Users/toddb/Desktop/LibraryScrape/LibraryData", row.names = FALSE)
  }
  
  #Recursive function to run program and plot data
  repeatData = function(interval = 10) {
    
    #Scrapes new data
    getNewData()
    
    #Retrieves data
    Libdata <- read.csv("/Users/toddb/Desktop/LibraryScrape/LibraryData")
    Libdata$Datetime <- as_datetime(Libdata$Datetime)
    
    #Plots the data
    output$tracker <- renderPlot({
      ggplot(read.csv("/Users/toddb/Desktop/LibraryScrape/LibraryData"), 
             aes(Datetime, Occupancy, group = 1)) +
        geom_line(col = "purple") +
        labs(x = "Date and Time", y = "Occupancy", 
             title = "The Occupancy of the Bill Bryson Library")
    }, res = 96)
    
    #Repeats again after specified time interval
    later::later(repeatData, interval)
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)
