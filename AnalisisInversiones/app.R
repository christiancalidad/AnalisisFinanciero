library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(quantmod)
library(tidyverse)
library(shinyWidgets)

ui <- navbarPage("Dashboard para Analisis de Inversiones",
                 tabPanel("Dashboard S&P500", 
                          fluidPage(
                     
                            setBackgroundColor("darkkhaki"),
            
                
                            fluidRow(column(4,wellPanel(
                
                                dateInput(inputId = "Fecha1", "Visualizar desde", value = "2016-01-01", min = "2016-01-01", max = "2020-10-01", format = "yyyy-mm-dd", language = "es"),
                                dateInput(inputId = "Fecha2", "Visualizar hasta", value = Sys.Date(), min = "2016-01-02", max = Sys.Date(), format = "yyyy-mm-dd", language = "es"),
                                selectInput(inputId = "Periodo", "Periodicidad", c("Diario"="Diario", "Mensual"="Mensual","Anual"="Anual")))),
            
                                
                                column(4,
                                       selectInput(inputId = "Company", "Company", c("Amazon"="Amazon", "Apple"="Apple","Jhonson"="JyJ","Microsoft"="Microsoft","Procter"="PyG")),
                                       selectInput(inputId = "Grafico", "Grafico", c("Line Chart"="Line", "OHLC"="OHLC")),
                                       checkboxInput("Low", label = "Low Value", value = TRUE),
                                       checkboxInput("High", label = "High Value", value = TRUE))),
                            
                            hr(),
                            column(12,
                                
                            plotlyOutput("SerieSP"))
                            )
                          ),
                 tabPanel("DashBoard TRM y Petroleo", 
                          fluidPage(setBackgroundColor("darkkhaki"),
                                    
                                    
                                    fluidRow(column(4,wellPanel(
                                        
                                        dateInput(inputId = "Fecha3", "Visualizar desde", value = "2008-01-01", min = "2008-01-01", max = "2020-10-01", format = "yyyy-mm-dd", language = "es"),
                                        dateInput(inputId = "Fecha4", "Visualizar hasta", value = Sys.Date(), min = "2008-01-02", max = Sys.Date(), format = "yyyy-mm-dd", language = "es"),
                                        selectInput(inputId = "Periodo2", "Periodicidad", c("Diario"="Diario", "Mensual"="Mensual","Anual"="Anual")))),
                                        
                                        
                                        column(4,
                                               selectInput(inputId = "Indicador", "Indicador", c("TRM"="TRM", "WTI"="WTI")),
                                               checkboxInput("Low2", label = "Low Value", value = TRUE),
                                               checkboxInput("High2", label = "High Value", value = TRUE))),
                                    
                                    hr(),
                                    column(12,
                                           
                                           plotlyOutput("SerieTRM"))
                          )
                              
                              
                              
                 )
                 )

##### Generar informacion de SP500

## Download TRM from Yahoo Api
Apple <- getSymbols("AAPL",auto.assign=FALSE, from="2016-01-01", src='yahoo')
Microsoft <- getSymbols("MSFT",auto.assign=FALSE, from="2016-01-01", src='yahoo')
Amazon<- getSymbols("AMZN",auto.assign=FALSE, from="2016-01-01", src='yahoo')
JyJ<- getSymbols("JNJ",auto.assign=FALSE, from="2016-01-01", src='yahoo')
PyG<- getSymbols("PG",auto.assign=FALSE, from="2016-01-01", src='yahoo')


## Transformar bases de datos a Data Frames

Apple <- data.frame(Date=index(Apple), coredata(Apple), Company = 'Apple')
Microsoft <- data.frame(Date=index(Microsoft), coredata(Microsoft), Company = 'Microsoft')
Amazon <- data.frame(Date=index(Amazon), coredata(Amazon), Company = 'Amazon')
JyJ <- data.frame(Date=index(JyJ), coredata(JyJ), Company = 'JyJ')
PyG <- data.frame(Date=index(PyG), coredata(PyG), Company = 'PyG')


colnames(Apple)=colnames(Microsoft )=colnames(Amazon)=colnames(JyJ )=colnames(PyG )

SP500<- rbind(Apple,Microsoft,Amazon,JyJ,PyG)

## Estandarizar nombres de columnas
names(SP500)<-c('Date','Open','High','Low','Close','Volume','Adjusted','Company')


## Poner columnas de mes y año

SP500$Month<- format(as.Date(SP500$Date), "%Y-%m")
SP500$Year<- format(as.Date(SP500$Date), "%Y")




##### Generar informacion de TRM y Petroleo


## Download TRM from Yahoo Api
TRM <- getSymbols("USDCOP=X",auto.assign=FALSE, from="2008-01-01", src='yahoo')
WTI <- getSymbols("WTI",auto.assign=FALSE, from="2008-01-01", src='yahoo')


## Estandarizar nombres de columnas
names(TRM)<-c('Open','High','Low','Close','Volume','Adjusted')
names(WTI)<-c('Open','High','Low','Close','Volume','Adjusted')


## Transformar bases de datos a Data Frames

TRM <- data.frame(Date=index(TRM), coredata(TRM))
WTI <- data.frame(Date=index(WTI), coredata(WTI))

##Fill NAs
TRM<-TRM %>% fill(Open, High, Low, Close,Volume, Adjusted)

## Poner columnas de mes y año

TRM$Month<- format(as.Date(TRM$Date), "%Y-%m")
TRM$Year<- format(as.Date(TRM$Date), "%Y")
WTI$Month<- format(as.Date(WTI$Date), "%Y-%m")
WTI$Year<- format(as.Date(WTI$Date), "%Y")

## Reemplazaremos los valores errones con el precio de apertura el dia

TRM <- TRM %>% mutate(Close= ifelse(Close < 1000, Open, Close)) %>% 
    mutate(Low= ifelse(Low < 1000, Close, Low)) %>% 
    mutate(Adjusted= ifelse(Adjusted < 1000, Open, Adjusted)) 

## Unir DataFrames

TRM$Indicador = "TRM"
WTI$Indicador = "WTI"

DataTRM<- rbind(TRM,WTI)




server <- function(input, output) {
    
    
    
    output$SerieSP <- renderPlotly({ 
        
        sptemp <- filter(SP500, Company == input$Company)
        sptemp <- filter(sptemp,Date>=input$Fecha1 & Date<=input$Fecha2)
        
        if(input$Periodo == "Diario"){
            temp<-sptemp
            
        }
        if(input$Periodo == "Mensual"){
            temp<-group_by(sptemp,Month) 
            temp<-summarize(temp, last(Close),max(Close), min(Close),first(Close))
            names(temp)<-c('Date','Close','High','Low','Open')
            
        }
        if(input$Periodo == "Anual"){
            temp<-group_by(sptemp,Year)
            temp<-summarize(temp, last(Close),max(Close), min(Close),first(Close))
            names(temp)<-c('Date','Close','High','Low','Open')
            
        }
        if(input$Grafico == 'Line'){
            fig <- plot_ly(temp, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines',name = 'Close')
            
            if(input$Low == TRUE){
                fig <-fig %>% add_trace(y = ~Low, name = 'Low', line = list(color = I('red'), width = 1))
            }
            if(input$High == TRUE){
                fig <-fig %>% add_trace(y = ~High, name = 'High', line = list(color = I('green'), width = 1))
            }
        }else{
            fig <- plot_ly(temp, x = ~Date, type="ohlc",
                           open = ~Open, close = ~Close,
                           high = ~High, low = ~Low) 
            
        }
            
        
        fig <- fig %>% layout(title = paste("Accion S&P500 - ", input$Company,  input$Periodo),
                              xaxis = list(title = "Fecha"),
                              yaxis = list (title = "Valor USD"))
        
        fig
        
    })
    
    
    output$SerieTRM <- renderPlotly({ 
        
        trmtemp <- filter(DataTRM, Indicador == input$Indicador)
        trmtemp <- filter(trmtemp,Date>=input$Fecha3 & Date<=input$Fecha4)
        
        if(input$Periodo2 == "Diario"){
            temp<-trmtemp
            
        }
        if(input$Periodo2 == "Mensual"){
            temp<-group_by(trmtemp,Month) 
            temp<-summarize(temp, last(Close),max(Close), min(Close))
            names(temp)<-c('Date','Close','High','Low')
            
        }
        if(input$Periodo == "Anual"){
            temp<-group_by(trmtemp,Year)
            temp<-summarize(temp, last(Close),max(Close), min(Close))
            names(temp)<-c('Date','Close','High','Low')
            
        }
        
        
        
        fig <- plot_ly(temp, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines',name = 'Close')
        
        if(input$Low2 == TRUE){
            fig <-fig %>% add_trace(y = ~Low, name = 'Low', line = list(color = I('red'), width = 1))
        }
        if(input$High2 == TRUE){
            fig <-fig %>% add_trace(y = ~High, name = 'High', line = list(color = I('green'), width = 1))
        }
        
        
        fig <- fig %>% layout(title = paste("Serie ",input$Indicador, " ",input$Periodo2),
                              xaxis = list(title = "Fecha"),
                              yaxis = list (title = "Valor $"))
        
        fig
        
    })
    
    
}


shinyApp(ui = ui, server = server)

