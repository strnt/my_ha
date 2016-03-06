#this is a home assignment for 'R-Track' course, Winter, CEU, 2016
#done by ak
#task: create a shiny application for exploratory data analysis including
## inputs to filter data on date and distance 
## static plots 
## HTML table 

library(shiny)
library(nycflights13)
library(data.table)

flights <- data.table(nycflights13::flights)

df <- merge(flights, airports, by.x = 'dest', by.y = 'faa', all.x = TRUE)
df$date <- as.Date(with(df, paste(year, month, day, sep="-")), "%Y-%m-%d")
df <- merge(df, airports, by.x = 'origin', by.y = 'faa', all.x = TRUE)
df <- merge(df, airlines, by.x = 'carrier', by.y = 'carrier', all.x = TRUE)
setnames(df, 'name', 'carrier_name')
df <- merge(df, planes, by.x = 'tailnum', by.y = 'tailnum', all.x = TRUE)
setnames(df, 'year.y', 'plane_year')
weather <- data.table(nycflights13::weather)
setorder(weather, year, month, day, hour)
##  aggregate the `weather` dataset and store as `daily_temperatures` to show the daily average temperatures based on the `EWR` records
daily_temperatures <- weather[, .(avg_temp = mean(temp)), by = .(year, month, day)]

##  merge the `daily_temperatures` dataset to `flights` on the date
setkey(daily_temperatures, year, month, day)
setkey(flights, year, month, day)
wf <- daily_temperatures[flights]

wf$date <- as.Date(with(wf, paste(year, month, day, sep="-")), "%Y-%m-%d")
wf <- wf[,c('date','avg_temp'), with = FALSE]
wf<-subset(wf,!duplicated(wf$date))

df <- merge(df, wf, by.x = 'date', by.y = 'date', all.x = TRUE, all.y = TRUE)


df[,year.x:=NULL]
df[,month:=NULL]
df[,day:=NULL]

toselect <- c('avg_temp', 'distance','air_time','dep_delay','arr_delay')

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Reactive data on flights departing NYC in 2013"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("dist",
                  "Distance:",
                  min = 0,
                  max = 5000,
                  value = df$distance),
      #selectInput('x', 'x', choices = c('arr_time', 'dep_time' )),
      #selectInput('y', 'y', choices = c('arr_delay', 'dep_delay')) ,
      
      dateRangeInput('daterange', 'Date range:', start = as.Date( head(df$date,1)), end = as.Date( tail(df$date,1)))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = 'pills',
                  tabPanel('Plot 1', plotOutput("reactplot")),
                  tabPanel('Plot 2', plotOutput('reactplot2')),
                  tabPanel('Table', dataTableOutput('datatable'))
                  
                  
      )
    )
  )))
