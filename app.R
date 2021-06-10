library(shiny)
library(shinydashboard)
library(readr)
library(plotly)
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(png)


# Define UI for application
ui <- 
  dashboardPage(skin = "black",
  
  #Define dashboard header  
  dashboardHeader(title = span("Elon Tweets Crypto",style = "font-size: 16px"), titleWidth = 185 ),
    
  #Define dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
            menuItem("Analysis", tabName = "analysis", icon=icon("graph")),
            menuItem("About", tabName = "about", icon=icon("about"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      #Defining analysis tab
      tabItem("analysis",
              
            column(width = 3,
                   
                   #UI box for tweet info
                   box(
                     title = "Tweet Info", width = NULL, status = "primary", solidHeader = TRUE,
                     selectInput("tweetChoice", "Select Tweet", choices =NULL),
                     "Tweet date:",textOutput("date"),br(),"Tweet target:",
                     textOutput("target"),br(),"Tweet Summary:",textOutput("summary")),
                   
                   #UI box for percentage change
                   box(valueBoxOutput("changePercent",width = 300),height = 130,width = NULL),
                   
                   #UI box for max value of cryptocurrency
                   box(valueBoxOutput("maxValue",width = 300),height = 130,width = NULL),
                  
                   #UI box for minimum value of cryptocurrency
                   box(valueBoxOutput("minValue",width = 300),height = 130,width = NULL),
                 
                   #UI box for hint
                   box(
                    width = NULL, background = "purple",
                    "HINT: Set the cryptocurrency option the same as the target coin mentioned in tweet info to see the effect of the tweet clearly")),
            
            column(width = 8,
                   
                   #UI box for selecting cryptocurrency
                   box(
                     status = "warning", width = NULL,
                     radioButtons(inputId = "coinChoice", label = "Cryptocurrency",
                                  choices = c("Bitcoin", "Ethereum", "Dogecoin"),inline = TRUE,selected=("Dogecoin"))),
                   
                   #UI box for timeseris graphs
                   tabBox(width = NULL,height = "500px",
                                  #Tab for candle stick plot
                                  tabPanel("Candlestick Chart",plotlyOutput("graph3",height = "400px")),
                                  #Tab for line chart
                                  tabPanel("Line Chart",box(plotlyOutput("graph1",height = "420px",width = 1000)))),
                   
                   #UI box for trade and volume bar graphs
                   tabBox(width = NULL,height = "250px",
                          tabPanel("Trades",plotlyOutput("graph2",height = "200px")),
                          tabPanel("Volume",box(plotlyOutput("graph4",height = "200px",width = 1020)))))
            
            ),
            
            
      #Defining about tab
      tabItem("about", 
              column(width = 8,
                     
              #UI box for objective
              box( title = "Objective :", solidHeader = TRUE, status = "primary",
               
                p("The main objective of this project is to portray the amount of effect of a single tweet by Elon Musk over the cryptocurrency market. 
                  Elon musk as of right now has over 56.8 Million followers on his Twitter account. This means that his single tweet has the potential to 
                  reach and influence the minds of more than 50 million people worldwide with less than just 280 characters. Along with that, the cryptocurrency 
                  market has become a new trend in recent years among the young generation who generally have lack of knowledge in investing market.")),
              
              #UI box for references
              box( title = "Refrences :", solidHeader = TRUE, status = "primary",
                tags$ul(
                  tags$li("Musk, E., 2021. [online] Available at: <https://twitter.com/elonmusk/status/1392030108274159619> [Accessed 5 June 2021]."), 
                  tags$li("Musk, E., 2021. [online] Available at: <https://twitter.com/elonmusk/status/1400620080090730501> [Accessed 5 June 2021]."), 
                  tags$li("Musk, E., 2021. [online] Available at: <https://twitter.com/elonmusk/status/1399985389725753346> [Accessed 5 June 2021]."),
                  tags$li("Musk, E., 2021. [online] Available at: <https://twitter.com/elonmusk/status/1396916392629137409> [Accessed 5 June 2021]."),
                  tags$li("Musk, E., 2021. [online] Available at: <https://twitter.com/elonmusk/status/1392974251011895300> [Accessed 5 June 2021]."),
                  tags$li("Musk, E., 2021. [online] Available at: <https://twitter.com/elonmusk/status/1392602041025843203> [Accessed 5 June 2021]."),
                  tags$li("Musk, E., 2021. [online] Available at: <https://twitter.com/elonmusk/status/1355068728128516101> [Accessed 5 June 2021]."),
                  tags$li("Musk, E., 2021. [online] Available at: <https://twitter.com/elonmusk/status/1387290679794089986> [Accessed 5 June 2021]."),
                  tags$li("CryptoArchive. 2021. CryptoArchive. [online] Available at: <https://www.cryptoarchive.com.au/download> [Accessed 5 June 2021]."),
                  tags$li("Vox. 2021. Elon Musk says he's breaking up with bitcoin. [online] Available at: <https://www.vox.com/recode/2021/5/18/22441831/elon-musk-bitcoin-dogecoin-crypto-prices-tesla> [Accessed 4 June 2021]."),
                  tags$li("Ante, Lennart. February 3, 2021. How Elon Musk's Twitter Activity Moves Cryptocurrency Markets <https://ssrn.com/abstract=3778844>[Accessed 5 June 2021]"),
                  tags$li("Rstudio.github.io. n.d. Shiny Dashboard Structure. [online] Available at: <https://rstudio.github.io/shinydashboard/structure.html#boxes> [Accessed 6 June 2021]."),
                  )
                
              )
            )
        )
                
      )
    
    )
   
  )
  

# Define server logic 
server <- function(input, output,session) {
  
  #Import Bitcoin Data
  Bitcoin <- read_csv("btcfinal1.csv", col_types = cols(date = col_skip(), 
                                                                  datetime = col_datetime(format = "%d-%m-%Y %H:%M")))
  #Import Ethereum Data
  Ethereum <- read_csv("ethfinal1.csv", col_types = cols(date = col_skip(), 
                                                                    datetime = col_datetime(format = "%d-%m-%Y %H:%M")))
  #Import Dogecoin Data
  Dogecoin <- read_csv("dogefinal1.csv", col_types = cols(date = col_skip(), 
                                                                    datetime = col_datetime(format = "%d-%m-%Y %H:%M")))
  #Import Tweets Data
  tweets <-read_csv("tweets.csv",col_types = cols(Date = col_datetime(format = "%d-%m-%Y %H:%M")))
  
  updateSelectizeInput(session, 'tweetChoice',
                         choices = tweets$Tweet,
                         server = TRUE)
  
    #Display tweet date
    output$date <- renderText({ 
      data <- get(input$coinChoice)
      tweetSelected=input$tweetChoice
      tweetDate<-as.POSIXct(tweets$Date[tweets$Tweet==tweetSelected])
    paste(tweetDate)
    })
    
    #Display tweet target
    output$target <- renderText({ 
      data <- get(input$coinChoice)
      tweetSelected=input$tweetChoice
      tweetTarget<-tweets$Target[tweets$Tweet==tweetSelected]
    paste(tweetTarget)
    })
    
    #Display tweet summary
    output$summary <- renderText({
    data <- get(input$coinChoice)
    tweetSelected=input$tweetChoice
    tweetSummary<-tweets$Summary[tweets$Tweet==tweetSelected]
    paste(tweetSummary)
    })

    #Render number of trades bar chart
    output$graph2<- renderPlotly({
      data <- get(input$coinChoice)
      tweetSelected=input$tweetChoice
      tweetDate<-as.POSIXct(tweets$Date[tweets$Tweet==tweetSelected])
      
      #Convert tweet date to GMT(-10 hrs[36000 sec] for Australia time) as cryptocurrency data is in GMT
      tweetDate2 <- tweetDate - 36000
      
      #Specify min max dates of graph
      minDate<-tweetDate2 - 3600
      maxDate<-tweetDate2 + 3600
      
      #Subset data from min to max dates
      subsetData <- subset(data, datetime >=minDate &
                             datetime <=maxDate)
      tweettrade <- subsetData$trades[subsetData$datetime==tweetDate2]
      
      x <- list(
        title = "Date"
      )
      y <- list(
        title = "Number of Trades"
      )
      
      a <- list(
        x = tweetDate2,
        y = tweettrade,
        text = "Tweet Point",
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 7,
        ax = -40,
        ay = -40
      )
      #Create the bar graph
      plott<- plot_ly(x = subsetData$datetime, y=subsetData$trades,type = 'bar')
      plott <- plott %>% layout(xaxis = x, yaxis = y,title="Number of Trades per minute")
      plott <- plott %>% layout(annotations = a)
      plott
      
      
    })
    
    #Render time series candle stick chart of open price
    output$graph3<- renderPlotly({
      data <- get(input$coinChoice)
      tweetSelected=input$tweetChoice
      tweetDate<-as.POSIXct(tweets$Date[tweets$Tweet==tweetSelected])
      
      #Convert tweet date to GMT(-10 hrs[36000 sec] for Australia time) as cryptocurrency data is in GMT
      tweetDate2 <- tweetDate - 36000
      
      #Specify min max dates of graph
      minDate<-tweetDate2 - 3600
      maxDate<-tweetDate2 + 3600
      
      #Subset data from min to max dates
      subsetData <- subset(data, datetime >=minDate &
                             datetime <=maxDate)
      tweetopen <- subsetData$open[subsetData$datetime==tweetDate2]
      
      x <- list(
        title = "Date"
      )
      y <- list(
        title = "Open Price in US$"
      )
      
      a <- list(
        x = tweetDate2,
        y = tweetopen,
        text = "Tweet Point",
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 7,
        ax = -60,
        ay = -80
      )
      
      
      
      #Create the candle stick graph
      fig <- subsetData %>% plot_ly(x = subsetData$datetime, type="candlestick",
                            open = subsetData$open, close = subsetData$close,
                            high = subsetData$high, low = subsetData$low) 
      fig <- fig %>% layout(title = "Trend of crytocurrency when elon musk tweets",
                            xaxis = list(rangeslider = list(visible = F)))
      fig <- fig %>% layout(xaxis = x, yaxis = y)
      
      fig <- fig %>% layout(annotations = a)
      
      fig
      
    })
        
    #Render time series line chart of open price
    output$graph1 <- renderPlotly({
      data <- get(input$coinChoice)
      tweetSelected=input$tweetChoice
      tweetDate<-as.POSIXct(tweets$Date[tweets$Tweet==tweetSelected])
      
      #Convert tweet date to GMT(-10 hrs[36000 sec] for Australia time) as cryptocurrency data is in GMT
      tweetDate2 <- tweetDate - 36000
      
      #Specify min max dates of graph
      minDate<-tweetDate2 - 3600
      maxDate<-tweetDate2 + 3600
      
      #Subset data from min to max dates
      subsetData <- subset(data, datetime >=minDate &
                             datetime <=maxDate)
      tweetopen <- subsetData$open[subsetData$datetime==tweetDate2]
      
      maxopen <- max(subsetData$open, na.rm = TRUE)
      
      #Create the line graph
      ggplot(subsetData, aes_string(x=subsetData$datetime, y=subsetData$open))+
        
        geom_line(color = "red", size = 0.3)+                 #Create a line graph 
        
        theme_bw()+ 
        
        scale_x_datetime(date_breaks = "30 min",date_labels = "%d %b %H:%M")+
        
        annotate(geom = "point", x = as.POSIXct(tweetDate2), y = tweetopen,colour = "black")+
        
        geom_text(aes(x=as.POSIXct(maxDate-1000), label="  = Tweet point", y=maxopen), colour="black", vjust  =
                    -0.5,size=3)+
        
        annotate(geom = "point", x = as.POSIXct(maxDate-1380), y = maxopen,colour = "black")+
        
        labs(title = "Trend of cryptocurrency when Elon Musk tweets.",y = "Open Price in US$", x = "Date")+
        
        theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              axis.line = element_line(colour ="black"),axis.text.x=element_text(angle=45,hjust=1),
              plot.caption=element_text(hjust=0.5,face="italic", color="#939393"))
    })
    
    #Render number of volume bar chart
    output$graph4<- renderPlotly({
      data <- get(input$coinChoice)
      tweetSelected=input$tweetChoice
      tweetDate<-as.POSIXct(tweets$Date[tweets$Tweet==tweetSelected])
      
      #Convert tweet date to GMT(-10 hrs[36000 sec] for Australia time) as cryptocurrency data is in GMT
      tweetDate2 <- tweetDate - 36000
      
      #Specify min max dates of graph
      minDate<-tweetDate2 - 3600
      maxDate<-tweetDate2 + 3600
      
      #Subset data from min to max dates
      subsetData <- subset(data, datetime >=minDate &
                             datetime <=maxDate)
      tweetvolume <- subsetData$volume[subsetData$datetime==tweetDate2]
      
      x <- list(
        title = "Date"
      )
      y <- list(
        title = "Volume"
      )
      
      a <- list(
        x = tweetDate2,
        y = tweetvolume,
        text = "Tweet Point",
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 7,
        ax = -40,
        ay = -40
      )
      
      #Create the bar graph
      plott<- plot_ly(x = subsetData$datetime, y=subsetData$volume,type = 'bar')
      plott <- plott %>% layout(xaxis = x, yaxis = y,title="Volume traded per minute")
      plott <- plott %>% layout(annotations = a)
      plott
      
      
      
    })
    
    #Display the minimum value of cryto
    output$minValue <-renderValueBox({
      
      data <- get(input$coinChoice)
      tweetSelected=input$tweetChoice
      tweetDate<-as.POSIXct(tweets$Date[tweets$Tweet==tweetSelected])
      
      #Convert tweet date to GMT(-10 hrs[36000 sec] for Australia time) as cryptocurrency data is in GMT
      tweetDate2 <- tweetDate - 36000
      
      #Specify min max dates of graph
      minDate<-tweetDate2 - 3600
      maxDate<-tweetDate2 + 3600
      
      #Subset data from min to max dates
      subsetData <- subset(data, datetime >=minDate &
                             datetime <=maxDate)
      tweetopen <- subsetData$open[subsetData$datetime==tweetDate2]
      
      
      min_v <- min(subsetData$open, na.rm = TRUE)
      
      valueBox(
        paste0(round(min_v,digits=3),"$"),
        subtitle = "Minimum value", 
        icon = icon("arrow-down"),
        color = "red"
      )
    })
    
    #Display the maximum value of cryto
    output$maxValue <-renderValueBox({
      
      data <- get(input$coinChoice)
      tweetSelected=input$tweetChoice
      tweetDate<-as.POSIXct(tweets$Date[tweets$Tweet==tweetSelected])
      
      #Convert tweet date to GMT(-10 hrs[36000 sec] for Australia time) as cryptocurrency data is in GMT
      tweetDate2 <- tweetDate - 36000
      
      #Specify min max dates of graph
      minDate<-tweetDate2 - 3600
      maxDate<-tweetDate2 + 3600
      
      #Subset data from min to max dates
      subsetData <- subset(data, datetime >=minDate &
                             datetime <=maxDate)
      tweetopen <- subsetData$open[subsetData$datetime==tweetDate2]
      
      max_v <- max(subsetData$open, na.rm = TRUE)
      
      valueBox(
        paste0(round(max_v,digits=3),"$"),
        subtitle = "Maximum value", 
        icon = icon("arrow-up"),
        color = "green"
      )
    })
    
    #Display the change percentage value of cryto since tweet
    output$changePercent <-renderValueBox({
      
      data <- get(input$coinChoice)
      tweetSelected=input$tweetChoice
      tweetDate<-as.POSIXct(tweets$Date[tweets$Tweet==tweetSelected])
      
      #Convert tweet date to GMT(-10 hrs[36000 sec] for Australia time) as cryptocurrency data is in GMT
      tweetDate2 <- tweetDate - 36000
      
      #Specify min max dates of graph
      minDate<-tweetDate2 - 3600
      maxDate<-tweetDate2 + 3600
      
      #Subset data from min to max dates
      subsetData <- subset(data, datetime >=minDate &
                             datetime <=maxDate)
      tweetopen <- subsetData$open[subsetData$datetime==tweetDate2]
      

      startopen <- subsetData$open[subsetData$datetime==tweetDate2]
      endopen <- subsetData$open[subsetData$datetime==maxDate]
      
      change <-((endopen - startopen)/endopen)*100
      
      valueBox(
        paste0(round(change,digits=2),"%"),
        subtitle = "Change Since Tweet", 
        icon = icon("arrows-v"),
        color = "orange"
      )
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
