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

    sidebarPanel
    (
      "Wybierz z listy:",
      br(),
      br(),
      actionButton("", label = "Plot1"),
      br(),
      br(),
      actionButton("", label = "Plot2"),
      br(),
      br(),
      actionButton("", label = "Plot3"),
      br(),
      br(),
      actionButton("", label = "Plot4"),
      br(),
      br(),
      actionButton("", label = "Plot5")
    
    ),
    
    # Show a plot of the generated distrisbution
    mainPanel(
      plotOutput("plotOne"),
      plotOutput("plotTwo"),
      plotOutput("plotThree"),
      plotOutput("plot4th"),
      plotOutput("plot5th")
    )
    
  
))