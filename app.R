library(shiny)
library(tidyverse)
library(plotly)
library(ggplot2)
library(data.table)

load('crimes.Rdata')
load('ind_crime.Rdata')
load('ind_c_u.Rdata')
load('panel2.Rdata')


# dt.crimes$Month = format.Date(as.Date(dt.crimes$Date), "%m") 

t2<-as.data.table(dt.crimes[Domestic=="false",.N,by=list(`Primary Type`,Year)][order(-N)])
t2<-t2[,SD:=N/max(N),by=`Primary Type`]

t3 = na.omit(dt.crimes[,.N,by=list(Ward,Year)])

ui <- fluidPage(
  column(12, align = 'center',
  titlePanel("Chicago Crime Analysis"),
  h2('Weijia Li'),
  h3('2018-05-21')),

  fluidRow(
    mainPanel(
      tabsetPanel(
        tabPanel(title='Introduction',
          p('Sixty-one victims have been shot in Chicago in the first six days of 2017, and 4368 were shot in 2016 alone, 
            according to [Chicago Tribune](http://crime.chicagotribune.com/chicago/). 
            In 2015, the most recent year for which data is available for both cities, the fatal shooting rate in Chicago 
            was five times as high as it was in New York: 15.6 per 100,000 residents compared to 2.8 per 100,000. According 
            to a report by the University of Chicago Crime Lab, there were 762 homicides in 2016, which was about 58 percent more 
            than in the previous year. But homicides were not the only category of crime to rise: other gun offenses, including 
            nonfatal shootings and robberies, soared.'),
          p('President Donald Trump has been targeting the gun violence in Chicago for his opinion on guns since the beginning of 
            his campaign. On Jan. 2nd, 2017, he tweeted: "the citys murder rate was `record setting` and suggested that Mayor 
            Rahm Emanuel should seek federal help if he canât do it himself. The underlying dynamics driving the change maybe too 
            complicated to reverse and to quantitatively understand. For ordinary people, the best we can do is to stay away from 
            troubles whenever we can. People who have stayed in a city should know where the troubles are. But what about 
            new-comers such as newly arrived international students from overseas who have little knowledge about Chicago? 
            As a student it might be beneficial to know how dangerous it is to be studying in Chicago and particularly what kind 
            of common senses we should employ if we were to live in a crime- filled metropolis.')
        ),
        tabPanel(title='By Type',
          uiOutput('Type'),
          h2('Distribution of Different Types of Crime across Weekdays and Years'),
          p('Please select a type of crime to view the distribution'),
          p('In the below heatmap, the darker the color represents a higher density of crime. By choosing
            different type of crime, we can see a clear pattern of how each crime distribute across weekdays and years.
            Looking at the heatmap, we find that not surprisingly, occurrences of alcohol-related violation are lowest on Mondays and
            highest on Fridays and Saturdays. However, surprisingly, for not obvious reasons, more homicides happen on Sundays and then
            Saturdays. Other crimes are fairly evenly distributed within each week.'),
          p('The interactive graph allow us to easily compare the occurance of different crimes over the week and years. I chose to
             present such information in a heatmap because the density of color is align with the density of crime occurance. Reading
             colors help the audiance to receive the information I wish to deliver.'),
          column(10,
          plotOutput('heat2'))),
        tabPanel(title='10-Year Trend',
          h2('Trends in Occurrence of Crimes in the Past 10 Years.'),
          column(10,
          p('Each row is normalized to its maximum value. We can see from this plot that despite most crimes stay stable or decrease a little over years, number of homicides skyrocketed in 2016. Sexual assaults, interference with public officer, offense involving children and deceptive practices have also been increasing.'),
          p('Again, a heatmap in yellow and green, which is a pair of contrast color, help the audience to quickly perceive the informatin
             displayed in the graph. The yellow blocks has a greater brightness that stands out from the other blocks; those blocks 
             shows a higher rate of crime. The interactivity in this heatmap allow the audiance to read detailed data of each block, 
             the parts that interest them.'),
          plotlyOutput('heat'))
        ),
        tabPanel(title='By Ward',
                 h2('The below graph shows the distribution of crime in different wards by years'),
                 p('The bubble plot highlights the significant data whilst minimise the distraction of other data on the graph.
                   The distribution is clearly displayed by size and color; the higher the crime rate, the larger 
                   and the greener are the bubbles. The interactivity of the graph allows the audiance to view clear bubble plots 
                   for each year.'),
                 p('From the graph, before 2015, Ward 28 always have the highest crime rate whereas 
                   Ward 42 became the highest. These Wards locate in the west and the north of Chicago.'),
                 sidebarLayout(position='right',
                  sidebarPanel(
                 radioButtons('Year','Year', choices = c('2007','2008','2009','2010','2011','2012','2013','2014',
                                                         '2015','2016','2017','2018'),
                              selected='2018')),
                 mainPanel(plotlyOutput('plot'))))
        )
      )
    )
)

server <- function(input, output) {
  output$heat <- renderPlotly({
    plot_ly(x = t2$Year, y = t2$`Primary Type`, z = t2$SD, 
            key = t2$SD, type = "heatmap", source = "heatplot") %>%
      layout(xaxis = list(title = "Year"), 
             yaxis = list(title = ""))
  })
  
  filtered <- reactive({
    na.omit(panel2) %>%
      filter(
        Year==Year,
        Type==input$Type,
        SD == SD,
        WD == WD
      )
  })
  
  output$Type <- renderUI({
    selectInput("Type", "Type",
                sort(unique(panel2$Type)),
                selected = "Homicide")
  })

  output$heat2 = renderPlot({
    ggplot(filtered(), aes(WD, Year)) + geom_tile(aes(fill = SD),colour = "white") + 
    scale_fill_gradient(low = "white", high = "steelblue",limits=c(0,1))+
    scale_y_continuous(breaks=seq(2007,2018))+
    scale_x_discrete(limits=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  })
  
  filteredata = reactive({
    t3 %>%
      filter(
        Year==input$Year,
        N == N,
        Ward == Ward)
  })

  output$plot = renderPlotly({
    plot_ly(data=filteredata(),x=~Ward, y=~N, text=~paste('Ward ',Ward), type='scatter', mode='markers', color=~N, colors='PiYG',
            marker=list(size=~N/500),hoverinfo = 'text')%>%
      layout(xaxis = list(title = "Ward",range=c(1,50)), 
             yaxis = list(title = "Total Number of Crime"),range=c(1000,100000))
      
  })

  
}

shinyApp(ui = ui, server = server)



