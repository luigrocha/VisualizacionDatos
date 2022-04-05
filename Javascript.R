###################################################
#LIBRERIAS JAVASCRIPT 
###################################################
#En este laboratorio veremos: 

#1.- NVD3
#2.- Morris
#3.- HighCharts
#4.- Leaflet
#5.- RickShaw

#rCharts es un paquete de R para crear, personalizar y publicar visualizaciones
#javascript interactivas desde R utilizando una interfaz de trazado de estilo 
#lattice. Es como  una librer�a paraguas que permite  usar otras librer�as
#de bajo nivel para generar gr�ficos usando javascript.
#La idea de rchart es producir gr�ficos basados en librer�as de javascript
#esto provee que el gr�fico sea f�cilmente exportable  a una pagina web

#Rcharts soporta el uso de varias librer�as de nivel medio. 
#Para informaci�n de esta librer�a referirse a: https://rdrr.io/github/ramnathv/rCharts/src/R/Polycharts.R

#Dado que soporta varias librer�as de javascript, existe una funci�n distinta para
#renderizar el gr�fico en cada  librer�a 
#Las funciones son : 

#Polycharts -> rPlot()
#Morris -> mPlot()
#NVD3 -> nplot()
#xChart -> xPlot()              
#highCharts -> hplot()
#Leaflet -> Leaflet()
#rickshow -> RickShow$new()


#Los siguiente URLs  contienen  una resumen de las capacidades 
#https://www.rpubs.com/dnchari/rcharts
#https://ramnathv.github.io/rCharts/
#https://rcharts.readthedocs.io/en/latest/tutorials/index.html

#INSTALACI�N 
#Para instalar tenemos que bajarlo directamente de github y para esto  tenemos que
#tener la librer�a de devtools
#Si baja los paquetes en fuente (type="source") deber� tambi�n instalar Rtools para poder compilar
#Si baja en paquete en ejecutable, �stos est�n  compilados para  R3.6
#El siguiente URL le guiar� en la instalaci�n :https://cran.r-project.org/bin/windows/Rtools/

rm(list=ls())
require(devtools)
#install_github('ramnathv/rCharts' ,type="source",  force=TRUE)
require(rCharts)
require(dplyr)
require(ggplot2)
require(reshape2)
require(MASS)
require(datasets)
library(leaflet)
#install.packages('RCurl')
#install.packages('RJSONIO')
#install.packages('leaflet')

##############################################
#USANDO NVD3
##############################################
#Esta librer�a intenta de crear gr�ficos re utilizables y componentes de gr�ficos
#basados en d3.js. Esta es una colecci�n muy nueva de componentes, con el objetivo
#de mantener los componentes muy personalizables, alej�ndose de sus soluciones 
#r�gidas como Polycharts
#Permite los siguientes tipos: 
#Scatter Chart,Multibar Chart,Multibar Horizontal Chart,Pie Chart,Donut Chart,Line Chart,Line with Focus Chart,Stacked Area Chart,Multi Chart

#En el siguiente ejemplo vemos como el tipo de barras es seleccionable por el usuario 
#as� como los  subsets que se grafican 
data(HairEyeColor)
hairEye <- as.data.frame(HairEyeColor)
hairEye
head(hairEye)
data(iris)

names(iris)
n1<- nPlot(Freq ~ Hair, group = 'Eye',
           data = subset(hairEye, Sex == "Female"),
           type = 'multiBarChart')
n1


#Note los botones en la parte superior del gr�fico completamente operativos

#con print podemos generar/ver  el c�digo  y copiarlo si se desea incrustar en una p�gina 
n1$print("chart1") #chart1 es el nombre de la secci�n del html

#El mismo gr�fico con  barras horizontales
n2 <- nPlot(Freq ~ Hair, group = "Eye", data = hairEye, type = "multiBarHorizontalChart")
n2$print("chart2")
n2

#Veamos la funcionalidad de  tooltip en el siguiente gr�fico 
data(economics)
n3 <- nPlot(uempmed ~ date , data = economics, type = 'linePlusBarChart')
n3$print("chart3")
n3

#Veamos la funcionalidad de resaltado y subsetting en el siguiente gr�fico
#Pie chart con la selecci�n de  dataset, en este caso ~cyl indica que no existe una variable Y 
#por lo tanto esta sintaxis se utilizar�a cuando se desea  ver una sola variable.
#Note que podemos seleccionar los componentes del pieChart
n4 <- nPlot(~ cyl, data = mtcars, type = 'pieChart')
n4$print("chart4")
n4

#Note que en este gr�fico podemos seleccionar un subset de los datos 


#El siguiente gr�fico contiene tooltip y la capacidad de enfoque en un rango 
#con el apuntador,  una ventana en la traza inferior  para obtener el zoom en la traza superior
ecm <- reshape2::melt(economics[,c('date', 'uempmed', 'psavert')],id = 'date')
n5 <- nPlot(value ~ date, group = 'variable',
            data = ecm,
            type = 'lineWithFocusChart')
n5$print("chart5")
n5

#Cada tipo de gr�fico tiene un script base sobre el cual se parametriza 
# estos scripts base forman parte de la librer�a cuando se descarga
#pero pueden ser cambiados directamente en el archivo o mediante la funci�n setTemplate()
#estas librer�as est�n en C:\Users\usuario\Documents\R\win-library\4.1\rCharts\libraries\nvd3\layouts
n6 <- nPlot(value ~ date, group = 'variable', data = ecm, type = 'multiChart')
n6$set(multi = list(
  uempmed = list(type="area", yAxis=1),
  psavert = list(type="line", yAxis=2)
))
n6$setTemplate(script = system.file(
  "/libraries/nvd3/layouts/multiChart.html",
  package = "rCharts"
))
#n6$print("chart6")
n6

#Note que usted podr�a cambiar cualquier par�metro en este archivo o incluso incrementar
#funcionalidad (requiere  conocimiento de Javsscript) 


############################################
#USANDO MORRIS
###########################################
#Morris.js es la biblioteca que provee los gr�ficos de l�nea ,�rea , barras, y donuts.
#Es una API muy simple de usar.
#Veamos ejemplos con economics 
data(economics, package = "ggplot2")
View(economics)
#como economics es una tibble, vamos a convertirle en data frame
econ <- transform(economics, date = as.character(date))
head(econ)

m1 <- mPlot(x = "date", y = c("psavert", "uempmed"), type = "Line", data = econ)
m1$set(pointSize = 0, lineWidth = 1)
m1
m1$print("chart2")
print("<html>")



#############################################
#USANDO HIGHCHARTS
############################################
#Highcharts es una moderna biblioteca de gr�ficos multiplataforma basada
#en SVG (Scalable Vector Graphics) es un formato de imagen basado en texto
# Cada imagen SVG se define mediante un c�digo de marcado similar al HTML.
#El c�digo SVG se puede incluir directamente en cualquier documento HTML.
#Facilita la adici�n de gr�ficos interactivos a proyectos web y m�viles. 

data(survey)
View(survey)

#Este dataset contiene informaci�n sobre tama�o de la mano 
#NW.Hnd: tama�o de la mano que no escribe 
#Nw.hand : tama�o del me�ique
#Clap : mano con la que aplaude (arriba)
h1 <- hPlot(x = "Wr.Hnd", y = "NW.Hnd",
            data = survey,
            type = c("line", "bubble", "scatter"),
            group = "Clap",
            size = "Age"
)
h1

h1 <- hPlot(x = "Wr.Hnd", y = "NW.Hnd",
            data = survey,
            type = c( "scatter"),
            group = "Clap",
            size = "Age"
)
h1

h1 <- hPlot(x = "Wr.Hnd", y = "NW.Hnd",
            data = survey,
            type = c( "bubble"),
            group = "Clap",
            size = "Age"
)
h1

#Note que tooltip y selecci�n del subset est�n activos 


##########################################
#USANDO LEAFLET
##########################################
#Leaflet es la biblioteca JavaScript de c�digo abierto  para mapas 
#interactivos compatibles con dispositivos m�viles. 
#Es bastante liviana y tiene todas las funciones de mapas que
#la mayor�a de los analistas necesitan.
#Est� dise�ado teniendo en cuenta la simplicidad, el rendimiento y la
#facilidad de uso. Funciona de manera eficiente en todas las principales 
#plataformas m�viles y de escritorio, se puede ampliar con muchos 
#complementos, tiene una API hermosa, f�cil de usar y bien documentada, 
#y un c�digo fuente simple y legible al que es un placer contribuir. 


mapa1  <- leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
  addTiles()
mapa1

#La funci�n leaflet() es el constructor del mapa
#La funci�n addTiles() lo que hace es  conectarse a OpenStreetMap para obtener 
#informaci�n de calles 
#Note que este mapa inicialmente ya presenta un mapa del mundo y permite hace zoom 
#Importante notar que leaflet utiliza objetos sp 
#El mapa tiene ya incorporado la funci�n de zoom, los pol�gonos por cada pa�s y ciudades 
#principales 

map1 <- leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18)) %>% addTiles() %>%
  addMarkers(lat=-0.1022, lng= -78.5048 , popup = "<p> Mi Casa </p>")
map1 

#Note que el constructor puede tomar algunos argumentos como la capacidad de zoom 
#addMarkers permite incorporar  marcadores de posici�n dentro del mapa

#Si existen varios markers que se requieren puede crearse un data frame con la data
df <- data.frame(lat= c(-0.1022,-0.2095) , lng=c(-78.5048, -78.4918))
df %>% leaflet() %>% addTiles() %>% addMarkers()

#Cuando hay muchos marcadores es posible  clusterizarlos a fin de que el gr�fico no 
#aparezca muy recargado
df %>% leaflet() %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
#Note el efecto cuando hace zoom out

dfpop <- data.frame(casos=c(166,66,32,25, 21) , lat= c(-0.1807,-2.1894,-1.0547,-2.9001,-3.2581), 
                    lng=c(-78.4678,-79.889,-80.4525 ,-79.0059,-79.9554)     )
dfpop %>% leaflet() %>% addTiles() %>% addCircles(weight=1, radius = (dfpop$casos)*1000)




###################################################
#USANDO RICKSHAW
#####################################################
#rCharts nos da el poder de Rickshaw, es un framework simple
#para dibujar gr�ficos de datos de series de tiempo en una p�gina web,
#construido sobre la  biblioteca D3 . 
#Estos gr�ficos pueden funcionar con conjuntos de datos hist�ricos 
#est�ticos o datos vivos que se actualizan continuamente en tiempo real.

head(USPersonalExpenditure)
usp = reshape2::melt(USPersonalExpenditure)
#Este dataset consta de los gastos personales de los Estados Unidos de 1940 a 1960
#(en miles de millones de d�lares) en las categor�as;
#alimentos y tabaco,  hogar, medicina y salud, cuidado personal y educaci�n privada durante los a�os 1940, 1945, 1950, 1955 y 1960.

#Convertimos las d�cadas en  fechas  que le gusta a Rickshaw
usp$Var2 <- as.numeric(as.POSIXct(paste0(usp$Var2, "-01-01")))
View(usp)

p4 <- Rickshaw$new()
p4$layer(value ~ Var2, group = "Var1", data = usp, type = "area", width = 560)
# agrega una barra deslizante �til as� de f�cil
p4$set(slider = TRUE)
p4$print("chart6")
p4

#Note el gr�fico de �rea con la barra deslizante para seleccionar el �rea de inter�s
