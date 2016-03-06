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

library(nycflights13)
library(ggplot2)
library(ggthemes)
shinyServer(function(input, output) {
  
  #Reactive data frame that will dynamically change based on date range, day of week, and crime category chosen
  datatable <- reactive({    
    seqdates <- seq.Date(as.Date(input$daterange[1]), as.Date(input$daterange[2]), by = 'days')
    seqdistance <- seq(from =as.numeric( input$dist[1]), to = as.numeric( input$dist[2]))
    #subset data frame by date range
    df <- df[which((df$date %in% seqdates) & ( df$distance %in% seqdistance)),]                
    df
  })
  
  
  output$datatable <- renderDataTable({datatable()})
  
  #########################################
  data.r = reactive({
    a = subset(df, date %in% seq.Date(as.Date(input$daterange[1]), as.Date(input$daterange[2]), by = 'days'))
    
  })
  
  
  
  output$reactplot <- renderPlot({
    
    
    df<-data.r()
    
    s = ggplot(df, aes(x=date, y=avg_temp))  +   geom_boxplot() + theme_pander()
    
    print(s)
  })
  
  data.r2 = reactive({
    a2 = subset(df, distance %in% seq(from =as.numeric( input$dist[1]), to = as.numeric( input$dist[2])))
    return(a2)
  })
  
  output$reactplot2 <- renderPlot({
    
    df<-data.r2()
    qq = ggplot(df, aes(x=distance, y=dep_delay))  +  geom_point() + theme_pander()
    
    print(qq)
    
  }
  )
})
