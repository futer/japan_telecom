

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(xlsx) # to read excel files
library(ggplot2) # to plot
library(scales) # to describe values on the plot 2,000,000 instead of 2000000

dataFromExcel <- read.xlsx(file="japan_telecom_dane.xlsx", sheetIndex=1,header=TRUE)
dataFromExcel2 <- read.xlsx(file="japan_telecom_dane_perc.xlsx", sheetIndex=1,header=TRUE)
###FIRST PLOT#####

shinyServer(function(input, output) {

  df <- dataFromExcel
  df2 <- dataFromExcel2
  
  output$plotOne <- renderPlot({

    df$Date <- as.Date(as.character(df$Date), format="%Y-%m-%d")
    x <- df$Date # first column with Date
    y <- df[ , 2:length(df)] # (all columns from df without the first one, the first column was x = Date)
    plotGgplot <- ggplot() +
      geom_line(data = df, aes(x = x, y = y$nttdocomo_prepaid, color=" nttdocomo_prepaid "), linetype = 1, size = 1.6) +
      geom_line(data = df, aes(x = x, y = y$nttdocomo_postpaid, color=" nttdocomo_postpaid "), linetype = 1, size = 1.6) +
      geom_line(data = df, aes(x = x, y = y$softbank_prepaid, color=" softbank_prepaid "), linetype = 1, size = 1.6) +
      geom_line(data = df, aes(x = x, y = y$softbank_postpaid, color=" softbank_postpaid "), linetype = 1, size = 1.6) +
      geom_line(data = df, aes(x = x, y = y$kddi_prepaid, color=" kddi_prepaid "), linetype = 1, size = 1.6) +
      geom_line(data = df, aes(x = x, y = y$kddi_postpaid, color=" kddi_postpaid "), linetype = 1, size = 1.6) +
      ylab('Number of Subscribers') +
      xlab('Year') +
      scale_y_continuous ( labels = comma, breaks = seq(from=0,to=190000000,by=5000000)) +
      ggtitle("Subscribers in Japan for main privider and its' competition in 2000-2013") +
      theme(plot.title=element_text(size=8, face="bold",
                                    hjust = 0.5),
            axis.title=element_text(size=8))
    plotGgplot
  })
  
  #####Second PLOT######
  
  output$plotTwo <- renderPlot({
    
    df$Date <- as.Date(as.character(df$Date), format="%Y-%m-%d")
    x <- df$Date # first column with Date
    y <- df[ , 2:length(df)] # (all columns from df without the first one, the first column was x = Date)
    plotGgplot <- ggplot() +
      geom_line(data = df, aes(x = x, y = y$nttdocomo_postpaid, color=" nttdocomo_postpaid "), linetype = 1, size = 1.6) +
      geom_line(data = df, aes(x = x, y = y$softbank_postpaid, color=" softbank_postpaid "), linetype = 1, size = 1.6) +
      geom_line(data = df, aes(x = x, y = y$kddi_postpaid, color=" kddi_postpaid "), linetype = 1, size = 1.6) +
      ylab('Number of Subscribers') +
      xlab('Year') +
      scale_y_continuous ( labels = comma, breaks = seq(from=0,to=190000000,by=20000)) +
      ggtitle("Subscribers in Japan for main privider and its' competition in 2000-2013") +
      theme(plot.title=element_text(size=8, face="bold",
                                    hjust = 0.5),
            axis.title=element_text(size=8))
    plotGgplot
  })
  
  #######THIRT PLOT#######
  output$plotThree <- renderPlot({
    df.totalFor2014 <- data.frame(matrix(apply(df[, 2:length(df)], 2, sum), 1))
    colnames(df.totalFor2014) <- names(df[,2:length(df)])
    df.totalFor2014 <- transform(df.totalFor2014, TotalSubsc = apply(df.totalFor2014, 1, sum))
    df.totalFor2014 <- as.data.frame(apply(df.totalFor2014, 2, function(x) x/df.totalFor2014$TotalSubsc))
    df.totalFor2014 <- apply(df.totalFor2014, 2, round, digits=3)
    df.totalFor2014 <- as.data.frame(df.totalFor2014[-nrow(df.totalFor2014),])
    names(df.totalFor2014) <- "TotalFor2014"
    df.totalFor2014 <- cbind(rownames(df.totalFor2014), df.totalFor2014)
    colnames(df.totalFor2014)[1] <- "ServiceProviders"
    x.totalFor2014 <- df.totalFor2014[,1]
    y.totalFor2014 <- df.totalFor2014[,2]
    allSubsc2015MarketSharePie <- ggplot(df.totalFor2014,
                                         aes(x = "", y=TotalFor2014, fill = ServiceProviders)) +
      geom_bar(width=1, stat="identity") +
      coord_polar(theta = "y") +
      scale_x_discrete("") +
      ggtitle("Subscribers Market Share in Japan for Mobile Prepaid and Postpaid market and its' competition in 2000 - 2013") +
      theme(plot.title=element_text(size=8, face="bold",
                                    hjust = 0.5),
            axis.title=element_text(size=8))
    allSubsc2015MarketSharePie
  })
  
  ###4th Plots###
  
  output$plot4th <- renderPlot({
    df2.totalFor2014 <- data.frame(matrix(apply(df2[, 2:length(df2)], 2, sum), 1))
    colnames(df2.totalFor2014) <- names(df2[,2:length(df2)])
    df2.totalFor2014 <- transform(df2.totalFor2014, TotalSubsc = apply(df2.totalFor2014, 1, sum))
    df2.totalFor2014 <- as.data.frame(apply(df2.totalFor2014, 2, function(x) x/df2.totalFor2014$TotalSubsc))
    df2.totalFor2014 <- apply(df2.totalFor2014, 2, round, digits=3)
    df2.totalFor2014 <- as.data.frame(df2.totalFor2014[-nrow(df2.totalFor2014),])
    names(df2.totalFor2014) <- "TotalFor2014"
    df2.totalFor2014 <- cbind(rownames(df2.totalFor2014), df2.totalFor2014)
    colnames(df2.totalFor2014)[1] <- "ServiceProviders"
    x.totalFor2014 <- df2.totalFor2014[,1]
    y.totalFor2014 <- df2.totalFor2014[,2]
    allSubsc2015MarketSharePie <- ggplot(df2.totalFor2014,
                                         aes(x = "", y=TotalFor2014, fill = ServiceProviders)) +
      geom_bar(width=1, stat="identity") +
      coord_polar(theta = "y") +
      scale_x_discrete("") +
      ggtitle("Subscribers Market Share in Japan for Mobile Prepaid and Postpaid market and its' competition in 2000 - 2013") +
      theme(plot.title=element_text(size=8, face="bold",
                                    hjust = 0.5),
            axis.title=element_text(size=8))
    allSubsc2015MarketSharePie
  })
  
  ##plot 5h###
  output$plot5th <- renderPlot({
    
    df2$Date <- as.Date(as.character(df2$Date), format="%Y-%m-%d")
    x <- df2$Date # first column with Date
    y <- df2[ , 2:length(df2)] # (all columns from df without the first one, the first column was x = Date)
    plotGgplot <- ggplot() +
      geom_line(data = df2, aes(x = x, y = y$Nttdocomo__pre_per, color=" nttdocomo_prepaid "), linetype = 1, size = 1.6) +
      geom_line(data = df2, aes(x = x, y = y$Nttdocomo__pos_per, color=" nttdocomo_postpaid "), linetype = 1, size = 1.6) +
      geom_line(data = df2, aes(x = x, y = y$softbank_pre_per, color=" softbank_prepaid "), linetype = 1, size = 1.6) +
      geom_line(data = df2, aes(x = x, y = y$softbank_pos_per, color=" softbank_postpaid "), linetype = 1, size = 1.6) +
      geom_line(data = df2, aes(x = x, y = y$kddi_pre_per, color=" kddi_prepaid "), linetype = 1, size = 1.6) +
      geom_line(data = df2, aes(x = x, y = y$kddi_pos_per, color=" kddi_postpaid "), linetype = 1, size = 1.6) +
      ylab('Number of Subscribers') +
      xlab('Year') +
      scale_y_continuous ( labels = comma, breaks = seq(from=0,to=1,by=0.05)) +
      ggtitle("Subscribers in Japan for main privider and its' competition in 2000-2013") +
      theme(plot.title=element_text(size=8, face="bold",
                                    hjust = 0.1),
            axis.title=element_text(size=8))
    plotGgplot
  })
  
  
  ##Plot 6TH - scale##
  
  output$plot6th <- renderPlot({
    df$Date <- as.Date(as.character(df$Date), format="%Y-%m-%d")
    
    # standardize scale => scale function
    
    df.stand <- as.data.frame(cbind(df[,1], scale(df[,2:ncol(df)])))
    colnames(df.stand)[8] <- "Date"
    df.stand$Date <- as.Date(df.stand$Date, origin="1970-01-01")
    class(df.stand$Date)
    x.stand <- df.stand[,1]
    y.stand <- df.stand[,2:length(df.stand)]
    df$Total <- NULL
    df$Population<- NULL
    df$F.pop<- NULL
    df$M.pop<- NULL
    
    x <- x.stand
    y <- y.stand
    plotGgplot <- ggplot() +
      geom_line(data = df, aes(x = x, y = y$nttdocomo_prepaid, color=" nttdocomo_prepaid "), linetype = 1, size = 1.6) +
      geom_line(data = df, aes(x = x, y = y$nttdocomo_postpaid, color=" nttdocomo_postpaid "), linetype = 1, size = 1.6) +
      geom_line(data = df, aes(x = x, y = y$softbank_prepaid, color=" softbank_prepaid "), linetype = 1, size = 1.6) +
      geom_line(data = df, aes(x = x, y = y$softbank_postpaid, color=" softbank_postpaid "), linetype = 1, size = 1.6) +
      geom_line(data = df, aes(x = x, y = y$kddi_prepaid, color=" kddi_prepaid "), linetype = 1, size = 1.6) +
      geom_line(data = df, aes(x = x, y = y$kddi_postpaid, color=" kddi_postpaid "), linetype = 1, size = 1.6) +
      ylab('Number of Subscribers') +
      xlab('Year') +
      scale_x_continuous(labels = comma) +
      scale_y_continuous ( labels = comma, breaks = seq(from=0,to=190000000,by=5000000)) +
      ggtitle("Subscribers in Switzerland for Mobile Prepaid and Postpaid and its' competition in 2003-2015") +
      theme(plot.title=element_text(size=8, face="bold",
                                    hjust = 0.5),
            axis.title=element_text(size=8))
    plotGgplot
  })
})
