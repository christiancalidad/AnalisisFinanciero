## Cargar librerias

library(shiny)
library(plotly)
library(dplyr)
library(tidyverse)
library(polycor)
library(lubridate)
library(tidyr)
## Framework for Financial Data Extracting and modeling
library("quantmod")



## Descqargar TRM y el precio del petrole from Yahoo Api
TRM <- getSymbols("USDCOP=X",auto.assign=FALSE, from="2008-01-01", src='yahoo')
WTI <- getSymbols("WTI",auto.assign=FALSE, from="2008-01-01", src='yahoo')



## Estandarizar nombres de columnas
names(TRM)<-c('Open','High','Low','Close','Volume','Adjusted')
names(WTI)<-c('Open','High','Low','Close','Volume','Adjusted')


## Transformar bases de datos a Data Frames

TRM <- data.frame(Date=index(TRM), coredata(TRM))
WTI <- data.frame(Date=index(WTI), coredata(WTI))

## Inspeccionar NAs

colSums(is.na(TRM))
colSums(is.na(WTI))

filter(TRM, is.na(Open))


## Vemos que los datos del WTI estan completos, mientras que los de la TRM contienen algunos NA,
## al revisar en la web, estas fechas si tienen dato por lo cual se trata de un error en la BD,
## para efectos practicos de este ejercicio los reemplazaremos con el valor anterior, sin embargo,
## en el futuro seria necesario buscar una fuente adicional para cubrirlos

TRM<-TRM %>% fill(Open, High, Low, Close,Volume, Adjusted)


## Inspeccionar Data Frames

str(TRM)
str(WTI)



## Vemos que hay menos filas en WTI que en TRM, asi que revisaremos los valores faltantes en ambas



miss <- filter(TRM, !Date %in% WTI$Date)

tail(miss,20)

## Vemos que en la base de WTI no hay valores para los festivos en Estados Unidos,
## Mientras que en la de TRM si, por lo cual para efectos del analisis eliminaremos estos 
## Datos para contar con bases comparables, espeicalmente de cara al analisis de correlacion


TRM<- filter(TRM, !Date %in% miss$Date)

## Misma revision para revision de manera opuesta

miss<- filter(WTI, !Date %in% TRM$Date)

tail(miss,20)

## Por otro lado, vemos que en TRM estan todos los dias que estan en WTI


## Poner columnas de mes y año

TRM$Month<- format(as.Date(TRM$Date), "%Y-%m")
TRM$Year<- format(as.Date(TRM$Date), "%Y")
WTI$Month<- format(as.Date(WTI$Date), "%Y-%m")
WTI$Year<- format(as.Date(WTI$Date), "%Y")


## Graficas TRM y WTI

## GRafica TRM

fig <- plot_ly(TRM, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines')
fig


## Como podemos ver en la grafica hay algunos datos atipicos  y despues de consultar otras
## fuentes de datos se puede concluir que son datos erroneos en la BD de Yahoo, por lo cual
## sera necesario ajustarlos


errors<-filter(TRM, Low <1000)
errors

## Vemos que el error es solo en las columnas Low, Close y Adjusted, dado que son pocos datos
## no nos alteraran mucho el analisis y las podemos ajustar a partir de los datos que estan correctos

## Reemplazaremos los valores errones con el precio de apertura el dia

TRM <- TRM %>% mutate(Close= ifelse(Close < 1000, Open, Close)) %>% 
      mutate(Low= ifelse(Low < 1000, Close, Low)) %>% 
      mutate(Adjusted= ifelse(Adjusted < 1000, Open, Adjusted)) 
      

## GRafica TRM Diria


graficar<-function(data,periodo,tipo){
   fig <- plot_ly(data, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines',name = 'Close')
   fig <-fig %>% add_trace(y = ~Low, name = 'Low', line = list(color = I('red'), width = 1))
   fig <-fig %>% add_trace(y = ~High, name = 'High', line = list(color = I('green'), width = 1))
   fig <- fig %>% layout(title = paste("Serie ", tipo,  periodo),
                         xaxis = list(title = "Fecha"),
                         yaxis = list (title = "Valor"))
   
   fig
}


graficar(TRM,'Diaria','TRM')

## Grafica TRM Mensual con el ultimo de precios de cierre, y  minimo y maximo.

temp<-group_by(TRM,Month) 

temp<-summarize(temp, last(Close),max(Close), min(Close))
names(temp)<-c('Date','Close','High','Low')

graficar(temp,'Semanal','TRM')



## Grafica TRM Anual con con el ultimo de precios de cierre, y  minimo y maximo.

temp<-group_by(TRM,Year) 

temp<-summarize(temp, last(Close),max(Close), min(Close))
names(temp)<-c('Date','Close','High','Low')
graficar(temp,'Mensual','TRM')



## GRafica WTI

graficar(WTI,'Diaria','WTI')


## Grafica WTI Mensual con con el ultimo de precios de cierre, y  minimo y maximo.

temp<-group_by(WTI,Month) 

temp<-summarize(temp, last(Close),max(Close), min(Close))
names(temp)<-c('Date','Close','High','Low')

graficar(temp,'Mensual','WTI')



## Grafica WTI Anual con promedio de precios de cierre, minimo y maximo en precios de cierre

temp<-group_by(WTI,Year) 

temp<-summarize(temp, last(Close),max(Close), min(Close))
names(temp)<-c('Date','Close','High','Low')


graficar(temp,'Mensual','WTI')


## Boxplots por mes TRM


fig <- plot_ly(TRM, y = ~Close, color = ~Month, type = "box",boxpoints = FALSE)
fig <- fig %>% layout(title = "BoxPlot por meses TRM (precio Cierre)",
                      xaxis = list(title = "Fecha"),
                      yaxis = list (title = "Valor $"),showlegend = FALSE)

fig


## Boxplots por mes WTI


fig <- plot_ly(WTI, y = ~Close, color = ~Month, type = "box",boxpoints = FALSE)
fig <- fig %>% layout(title = "BoxPlot por meses WTI(precio Cierre)",
                      xaxis = list(title = "Fecha"),
                      yaxis = list (title = "Valor USD"),showlegend = FALSE)

fig


##  Correlacion entre TRM y WTI sobre precio de Cierre

DataSet<- merge(TRM,WTI,by.x='Date',by.y='Date',all=TRUE)

DataSet<-select(DataSet, Date, Close.x, Close.y,Year.x)
names(DataSet)<-c('Date','TRM','WTI','Year')



## Elaboraremos primero un scatter plot para ver graficamente la relacion entre las variables
## y el comportamiento con el paso de los años



p <- ggplot(DataSet, aes( WTI, TRM ) ) +
      geom_point(aes( colour = Year )) +
      geom_smooth(method = 'loess' )

fig <- ggplotly(p)
fig <- fig %>% layout(title = "TRM vs WTI",showlegend = FALSE)

fig

## Al observar la grafica anterior podemos notar que las dos variables estan estrechamente
## Sin embargoo, esta relacion no es lineal, sino exponencial. 


## Para analizar de una manera exploratoria inicial la relacion entre estas dos variables, 
## a pesar de su relacion aparentemente exponencial, transformaremos el WTI en su logaritmo
## Para asi lograr aproximar la relacion a lineal

DataSet$LogWTI<-log(DataSet$WTI)

log()



p <- ggplot(DataSet, aes( LogWTI, TRM ) ) +
      geom_point(aes( colour = Year )) +
      geom_smooth(aes( group = 1 ))

fig <- ggplotly(p)
fig <- fig %>% layout(title = "TRM vs WTI",showlegend = FALSE)

fig

cor(DataSet$TRM,DataSet$WTI,use = 'complete.obs')
cor(DataSet$TRM,DataSet$LogWTI,use = 'complete.obs')

## A partir del resultado anterior podemos concluir que las dos variables estan relacionadas fuertemente
## de manera inversa y aproximadamente exponencial, ya que obtuvimos un coeficiente de correlacion
## de -0.85
## manera 



### Tabla con valores Maximo, minimo y promedio por año


ResumenTRM<-group_by(TRM,Year) 

ResumenTRM<-summarize(ResumenTRM, mean(Close),max(Close), min(Close))
names(ResumenTRM)<-c('Año','Promedio','Maximo','Minimo')

ResumenTRM



## Parte 2

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



sptemp <- filter(SP500, Company == 'Apple')

## GRafica SP500 Diria

temp<-sptemp

fig <- plot_ly(temp, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines',name = 'Close')
fig <-fig %>% add_trace(y = ~Low, name = 'Low', line = list(color = I('red'), width = 1))
fig <-fig %>% add_trace(y = ~High, name = 'High', line = list(color = I('green'), width = 1))
fig <- fig %>% layout(title = "Accion SP500 por día",
                      xaxis = list(title = "Fecha"),
                      yaxis = list (title = "Valor $"))

fig



## Grafica TRM Mensual con promedio de precios de cierre, minimo y maximo en precios de cierre

temp<-group_by(sptemp,Month) 

temp<-summarize(temp, mean(Close),max(Close), min(Close))
names(temp)<-c('Date','Close','High','Low')


fig <- plot_ly(temp, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines',name = 'Close')
fig <-fig %>% add_trace(y = ~Low, name = 'Low', line = list(color = I('red'), width = 1))
fig <-fig %>% add_trace(y = ~High, name = 'High', line = list(color = I('green'), width = 1))
fig <- fig %>% layout(title = "S&P500 por mes con base en cierres ",
                      xaxis = list(title = "Fecha"),
                      yaxis = list (title = "Valor $"))

fig




## Grafica TRM Anual con promedio de precios de cierre, minimo y maximo en precios de cierre

temp<-group_by(sptemp,Year) 

temp<-summarize(temp, mean(Close),max(Close), min(Close))
names(temp)<-c('Date','Close','High','Low')


fig <- plot_ly(temp, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines',name = 'Close')
fig <-fig %>% add_trace(y = ~Low, name = 'Low', line = list(color = I('red'), width = 1))
fig <-fig %>% add_trace(y = ~High, name = 'High', line = list(color = I('green'), width = 1))
fig <- fig %>% layout(title = "S&P por año con base en cierres ",
                      xaxis = list(title = "Fecha"),
                      yaxis = list (title = "Valor $"))


fig


### Punto 2

## Lectura de los datos

Empresas<-read.csv('BASE.txt',sep = ';')
Monedas <-read.csv('Monedas.txt',sep = ';')

## Revision de la estructura de la informacion


str(Empresas)
str(Monedas)

## Inicialmente, vemos que la fecha quedo como un factor, por lo cual la converteriemos a formato
## fecha para facilitar su manipulacion

Empresas$Fecha<-dmy(Empresas$Fecha)
Monedas$Fecha<-dmy(Monedas$FECHA)
Monedas$FECHA<-NULL

## Ahora revisaremos el estado de los datos numericos


tapply(Empresas$Empresa, Empresas$Moneda, summary)

##Vemos que tenemos datos algunos registros de una empresa llamada "EmpresaC" en la Base datos
##, dado que la base datos solo debe contener informacion solo de 3 empresas, y que los registros
## de las otras 3 empresas estan completos(365 dias), asumiremos que estos datos de la "EmpresaC"
## se deben a un error en la generacion de la informacion y los eliminaremos

Empresas<-filter(Empresas,Empresa != 'EmpresaC')



temp<-group_by(Empresas,Empresa,Moneda)

summarize(temp, mean(Ingreso),max(Ingreso),min(Ingreso))

## Los valores aparentemente se ven correctamente leidos y sin datos atipicos

## Join para contar con toda la informacion en el mismo data frame

df <-merge(x = Empresas, y = Monedas, by = "Fecha", all.x = TRUE)


## Calcular valor en pesos para todos los registros

df <- df %>% mutate(IngresoPesos= case_when(Moneda == 'USD' ~ Ingreso*USD,
                                            Moneda == 'EURO' ~ Ingreso*EURO,
                                            Moneda == 'PESO' ~ Ingreso))


## Revisar que no hayan quedado NAs
colSums(is.na(df))


## Sub total de ingreso por empresa donde la monera de origen es USD

temp<-filter(df,Moneda == 'USD')
temp<-group_by(temp,Empresa)
data.frame(summarise(temp, IngresosUD =sum(Ingreso)))


## Ingreso total por moneda de la empresa A

temp<-filter(df,Empresa == 'Empresa A')
temp<-group_by(temp,Moneda)
data.frame(summarise(temp, IngresosUD =sum(Ingreso)))

## Empresa con Menor ingreso durante el primer semestre del 2017

temp<-filter(df,Fecha <=dmy('30/06/2017'))
temp<-group_by(temp,Empresa)
temp<- data.frame(summarise(temp, Ingresos =sum(IngresoPesos)))
temp[order(temp$Ingresos),]

## Ingresos para todos los trimes de 2017 en pesos para cada trimestre

temp<-df
temp$Trimestre<-quarter(temp$Fecha)
temp<-group_by(temp,Empresa,Trimestre)
data.frame(summarise(temp, Ingresos =sum(IngresoPesos)))


## Inreso acumulado del año 2017 en pesos por empresa y moneda


temp<-group_by(df,Empresa,Moneda)
data.frame(summarise(temp, Ingresos =sum(IngresoPesos)))

## Analisis y Conclusiion
## Para esto analiceremos los ingresos de las empresas por cada tipo de moneda, llevados a pesos.

data<-df
data$Mes<- format(as.Date(data$Fecha), "%Y-%m")
data<-select(data,Empresa,Moneda,Mes,IngresoPesos)
data<-group_by(data,Empresa,Moneda,Mes)
data<-data.frame(summarise(data, Ingresos =sum(IngresoPesos)))

p <- ggplot(data, aes(x=Mes, y=Ingresos,group=Moneda)) + geom_line(aes(color=Moneda))
p + facet_grid(rows = vars(Empresa))+ geom_smooth(method = "lm",se = FALSE,linetype = "dashed",
                                                  size=0.2)


## A partir del grafico anterior podemos concluir lo siguiente:

