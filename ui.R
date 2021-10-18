library(methods)
library(base)
library(stats)
library(datasets)

library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(broom)
library(gganimate)
library(transformr)
library(gifski)
library(shinybusy)

myfile <- "https://raw.githubusercontent.com/Xeryus01/data/main/UNVR_fix.csv"

data1 <- read_csv(myfile)
data1$tanggal <- as.Date(data1$tanggal, "%m/%d/%Y")

fluidPage(
  h1("Static Line Chart"),
  dateInput("plotFrom", "From:", value = "2016-01-04", min = "2016-01-04", max = "2021-08-31",format = "mm/dd/yyyy"),
  dateInput("plotTo", "To:", value = "2021-08-31", min = "2016-01-04", max = "2021-08-31", format = "mm/dd/yyyy"),
  
  hr(),
  div(plotOutput("plot_stat"), style="text-align: center;"),
  
  hr(),
  h1("Line Chart Race"),
  dateInput("dateFrom", "From:", value = "2016-01-04", min = "2016-01-04", max = "2021-08-31", format = "mm/dd/yyyy"),
  dateInput("dateTo", "To:", value = "2021-08-31", min = "2016-01-04", max = "2021-08-31", format = "mm/dd/yyyy"),
  br(),
  actionButton("race", "Submit"),
  
  hr(),
  div(imageOutput("plot"), style="text-align: center;"),
  
  add_busy_spinner(spin = "fading-circle")
)