##################################################
# LABORATORIO  INTERACCI�N WEB 
##################################################
#En este laboratorio veremos : 
#Como Leer y descargar data 
#Como realizar WebScraping 
#
#
#Existen m�ltiple paquetes R que permiten la interacci�n  directa con Internet  
#Sus diferencias son b�sicamente mecanismos de parsing, preproceso de la data 
#tipos de estructuras que generan, su eficiencia en trabajo con grades vol�menes y 
#la versi�n R con la que trabajan 
#Entre ellos tenemos : 
#utils
#readr
#iotools
#rvest 
#textreadr
#data.table

#cargando librerias 
rm(list=ls())
require(magrittr)
require(httr)
require(rvest)
require(dplyr)
require(XML)
require(xml2)
require(data.table)
install.packages('magrittr')
install.packages('httr')
install.packages('rvest')
install.packages('XML')
install.packages('magrittr')

###############################
#DESCARGA DE DATOS 
###############################
#Si lo que se desea es descargar  data directamente al disco, la mejor forma de hacerlo es : 

utils::download.file("http://www.jaredlander.com/data/ExcelExample.xlsx"
                     ,destfile="ejemplo" , method="auto", 
                     , mode="wb")

#El modo con el que crear� el archivo. Los valores  son "w" para escritura, "wb" escritura
#binario, "a" para incrementar y "ab" incremento binario .

#Esta funci�n es un wrapper (envoltorio) de otras funciones. El argumento method permite indicar 
#la librer�a que se usar� para el acceso. Las opciones son "internal", "wininet" (Windows ) 
#"libcurl", "wget" y "curl", los m�todos pueden cambiar seg�n la versi�n R: el m�todo libcurl 
#se introdujo en R 3.2.0 y todav�a es opcional en Windows: 

#########################################
#DESCARGANDO AL AMBIENTE R
#########################################

#Si lo que se desea es descargar al archivo y leerlo al ambiente de R, debemos usar funciones que 
#que luego de leer conviertan el archivo en un dataset de R. 
#Esto depende del formato en el que se halle el archivo originalmente
#Para  archivos del tipo CSV  podemos usar estas librer�as y funciones
#utils::read.csv()
#readr::read_delim()
#utils::read.table()
#iotools::read.csv.raw() :

#La funci�n read.csv lee el archivo y lo convierte a un data frame 

url <- "https://www.jaredlander.com/data/Federal_Stimulus_Data.csv"
fed <- read.csv(url)
View(fed)




#Esta funci�n trabaja bien , pero la librer�a es antigua y no esta optimizada para grandes
#vol�menes de data.
#read_table es m�s r�pido pero tambi�n m�s estricto, cada l�nea debe tener la misma longitud y 
#cada campo est� en la misma posici�n en cada l�nea. 
#Primero encuentra columnas vac�as y luego las analiza como un archivo de ancho fijo. 
#La data en el ambiente R ser� un tibble

tib1 <- readr::read_table(url, col_names = TRUE)

#Otra opci�n para velocidad es read.csv.raw
#Esta funci�n es un reemplazo r�pido de read.csv y read.delim que precarga los datos como
#un vector sin procesar y analiza sin construir cadenas intermedias, por lo que lo hace 
#muy r�pido


download.file("https://covid19.who.int/WHO-COVID-19-global-data.csv", "OMS_COVID.CSV")
covid_df <- iotools::read.csv.raw("OMS_COVID.CSV")
names(covid_df)

#La mejor  opci�n para velocidad es  fread de data.table 
tt <- data.table::fread("https://covid19.who.int/WHO-COVID-19-global-data.csv")


#########################################
#WEB   SCRAPPING
########################################

#Podemos tambi�n leer partes de la p�gina web como texto a pesar que no 
#podemos procesarla de esta forma. Pero es  interesante para  entender 
#la construcci�n de la p�gina. (Aunque posiblemente ser�a mejor usar F12 en el browser)

corona <- "https://github.com/nytimes/covid-19-data#coronavirus-covid-19-data-in-the-united-states"
texto <- readLines(corona)
cat(texto)

#Podemos bajar directamente una p�gina web a un  string con la funci�n GET de httr 
#El m�todo GET  recupera cualquier informaci�n (en forma de entidad) identificada por 
#el Request-URI. Si el Request-URI se refiere a un proceso de producci�n de datos, son los
#datos producidos los que se devolver�n como la entidad en la respuesta . 

puceeduec <- httr::GET("https://www.puce.edu.ec")
names(puceeduec)
#Y revisarla de esta forma 
str(puceeduec, max.level = 2)
str(puceeduec, max.level = NA)
#nivel m�ximo de anidamiento que se aplica para mostrar estructuras anidadas, por ejemplo, 
#una lista que contiene sublistas. NA predeterminado: muestra todos los niveles de anidamiento, 
#puedo ver los tags de la cabecera de http
puceeduec$headers
puceeduec$status_code
puceeduec$cookies
puceeduec$request

#la funci�n GET() devuelve una lista con las cabeceras del sitio adem�s
#del HTML. Para acceder al contenido html de la p�gina en s�, necesitamos usar
#la funci�n content()

str(content(puceeduec, type="text"), nchar.max=1000)

#Ya podemos acceder a alguna informaci�n pero todav�a falta automatizar el tema
#Para esto usaremos read_html de XML , para procesar el html
#read_html lee el contenido html. Esto es generalizado, se lee en todo el cuerpo del texto.
#Dependiendo de la construcci�n de la p�gina, el usuario podr�a necesitar los paquetes
#xml2 y rvest. 
#En general los pasos a seguir son : 
#1.- Obtener  el HTML de la p�gina web que desea acceder
#2.- Decidir  qu� parte de la p�gina desea leer y averig�e qu� HTML/CSS necesita para seleccionarla.
#3.- Seleccione el HTML y anal�celo de la forma que necesite 

#Leamos toda la p�gina 
#En este ejemplo queremos extraer data de la p�gina de pron�sticos del clima  
newyork <- rvest::read_html("https://forecast.weather.gov/MapClick.php?lat=40.71455000000003&lon=-74.00713999999994")
boston <- read_html("https://forecast.weather.gov/MapClick.php?lat=42.35866000000004&lon=-71.05673999999993")
sanfrancisco  <- read_html("https://forecast.weather.gov/MapClick.php?lat=37.7771&lon=-122.4196#.Xl0j6BNKhTY")
#Nos interesa la tabla de condiciones del clima  
#revisando la p�gina encontramos que las condiciones est�n en un tabla (de html)
#Para esto utilizaremos html_node
#Un nodo se refiere a un punto de la estructura de �rbol DOM 
#Una vez que tengamos todos estos nodos, podemos pasar la salida de html_nodes() a la funci�n
#html_text(). Necesitamos  obtener el texto real de la etiqueta <p>, por lo que esta funci�n ayuda con eso. 

#Podemos investigar un poco la p�gina 
divs <- newyork %>% html_element("div")   #note que no va con los brackets angulares
divs
ps <- newyork %>% html_nodes("p")   #note que no va con los brackets angulares
ps
tablas <-newyork %>% html_nodes("table")   #note que no va con los brackets angulares
tablas %>% html_table()

#De lo anterior ya podemos extraer un elemento
#Por ejemplo si queremos extraer la temperatura m�s baja 
maxtemp <- ps[[16]] %>% html_text() 
maxtemp

# O toda la tabla 
nodosSF <- html_node(sanfrancisco , "table" )
nodosNY <-  html_node(newyork , "table" )
nodosBoston  <- html_node(boston , "table" )
#Obtenemos solo el texto y lo pasamos a una tabla 
sfdata  <-   nodosSF %>%   html_text() 
sfdata
nydata  <-   nodosNY %>%   html_text() 
nydata
bostondata  <-  nodosBoston %>%   html_text() 
bostondata
#Estas variables ya pueden ser procesadas y desplegadas



#Veamos un  ejemplo donde no hay tablas sino secciones 
urlpizza<-"https://menupages.com/fiores-pizza/165-bleecker-st-new-york"
pizza1 <- read_html(urlpizza)

divs <- pizza1 %>% html_element("div")   
divs
ps <- pizza1 %>% html_nodes("p")  
ps
tablas <- pizza1 %>% html_nodes("table")  
tablas %>% html_table()

#Analizando el html encontramos la secci�n donde est�n los productos  y precios 
#ejemplo, cargue la p�gina en un browser para facilitar el entendimiento 
html_node(pizza1 , "div[class='menu-item-list']") 


#como en este caso no tenemos una tabla tendr�amos que buscar en cada secci�n, lo 
#cual puede ser un poco tedioso. Una forma m�s sencilla es hacer un parse del 
#html  para poder desplegarlo en un editor 

#La funci�n htmlParse() analiza un archivo XML o HTML o una cadena que contiene contenido
#XML/HTML y genera una estructura R que representa el �rbol XML/HTML. 
pagedoc<-XML::htmlParse(pizza1)
pagedoc

# Y ahora lo creamos como archivo para verlo en un editor cualquiera

f = system.file()
doc <- XML::htmlParse(pizza1)
sink("pizza1.txt")
doc
sink()

# Y ahora lo podemos ver con un editor cualquiera o en la consola 
#Ver con notepad


#De este archivo entonces es f�cil obtener la estructura y copiarla 
#xpathApply  intenta simplificar el resultado si se puede convertir a un vector o matriz 
#en lugar de dejarlo como una lista. De este modo, 
precio_ensalada_cesar <-xpathApply(pagedoc, "//div[@class='menu-item-list']/div[@id='item-12060742']/div[@class='menu-item__information']/span[@class='menu-item__price']",fun=xmlValue)
precio_ensalada_cesar

precio_margarita <-xpathApply(pagedoc, "//div[@class='menu-item-list']/div[@id='item-12060776']/div[@class='menu-item__information']/span[@class='menu-item__price']",fun=xmlValue)
precio_margarita
