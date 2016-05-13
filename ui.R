# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  headerPanel("Japan TeleCOM"),
  
  includeCSS("styles.css"),
  #
  # Application title
  titlePanel("Subscribers Market Share in Japan for Mobile Prepaid and Postpaid market and its' competition in 2000-2013"),
  
  sidebarPanel(
    selectInput("plotnumber", "Select plot", c("Plot1st", "Plot2nd", "Plot3th", "Plot4th", "Plot5th","Plot6th"), selected = "Plot1st")
  ),
  
  mainPanel(
    plotOutput("whichplot")
  )
))
