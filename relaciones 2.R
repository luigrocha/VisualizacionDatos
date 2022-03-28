#########################################################
#LAB_RELACIONES 2
#En este laboratorio veremos 
#1.-Diagramas de dispersi�n
#2.-Diagramas de correlaci�n
#3.-Dendrogramas
#4.-Diagramas de Conglomerados(Clusters)
####################################################
#Este laboratorio pretende revisar los posibles diagramas para visualizar 
#como dos o mas a variables num�ricas se relacionan entre si. 

rm(list=ls())
require(ggplot2)
require(reshape2)
require( viridisLite)
require(viridis)
#install.packages('mclust')
require(mclust)
require(knitr)


###############################################
# DIAGRAMAS DE DISPERSI�N
##############################################
# El primer diagrama que generalmente lo usamos en la fase exploratoria es el diagrama de dispersi�n
#El geom b�sico en este caso es geom_point
#En este primer ejemplo queremos establecer cual es la relaci�n entre hp y qsec
#utilizando gear como factores gear y vs. Como hay dos factores el mapeo est�tico 
#lo haremos con size y color
data(mtcars)
#View(mtcars)
names(mtcars)

ggplot (data=mtcars, aes(x=hp , y= qsec ))+
  geom_point(aes(color=vs,size=gear))

#El diagrama resultante es bueno pero entre m�s factores  se 
#incluyen m�s complicado de leer el diagrama ,a dem�s no hay mucho m�s mapeos posibles

#Una soluci�n si queremos ver en diagramas simples es  la funci�n pairs()
#nos permite automatizar el proceso de graficar las correlaciones.
#Aunque el resultado no es de lo mejor 

pairs(mtcars[1:4], main = "Relacion de variables ",  bg = c("red", "green3", "blue"),  pch = 23)

#Si bien el diagrama ha automatizado m�ltiples diagramas de dispersi�n, pero no es recomendado
#cuando queremos escalarlo 
pairs(mtcars[1:6], main = "Relacion de variables ",  bg = c("red", "green3", "blue"),  pch = 23)

#Otro ejemplo sobre el dataset Iris
pairs(iris[1:4], main = "Data Iris  ",
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

#pch especifica el tipo de icono del punto, bg es obviamente el color 
#El gr�fico anterior presenta una serie de mapeos en tama�o, color. Y puede
#tener muchos m�s, esto hace que el programa sea dif�cil de digerir 

#Una alternativa ser�a definir peque�os m�ltiples 

ggplot (data=mtcars, aes(x=hp , y= qsec ))+
  geom_point(aes(color=vs,size=gear))+
  facet_wrap(~vs+gear, labeller = "label_both")


#vs es motor en V(0) o en linea (1) 
#gear es numero de marchas 3,4,5
#Esos diagramas funcionan bien porque se puede observar la relaci�n entre variables
#pero el problema es que la cantidad de  m�ltiples es directamente proporcional con la
#la cantidad de niveles de cada factor. 
#Si tenemos 3 factores y cada uno tiene 4 niveles tendremos ya 12 gr�ficos. 

#En estos puede resultar �til realizar una reducci�n de dimensiones, por ejemplo, 
#en forma de an�lisis de componentes principales, pero para esto necesitamos 
#encontrar  las relaciones entre variables

###################################################
# DIAGRAMAS  DE CORRELACION 
###################################################

# Otra vez si hacemos una matriz de correlaci�n de 4 variables tendremos 16 valores
# y la mejor forma de graficar es mediante un diagrama de calor
#CALCULAMOS LA MATRIZ DE CORRELACION 

cor_mat <- round(cor(mtcars),2)
head(cor_mat)
#El formato de la matriz no aceptable para ggplot, vamos a transponer con la funci�n melt 

cor_mat_melted  <- melt(cor_mat)

#Ahora lo graficamos como  diagrama de calor 
#geom_tile sirve para hacer diagramas de calor en 2 dimensiones

ggplot(data = cor_mat_melted, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_viridis(discrete = FALSE, option = "A")

#El gr�fico no es  bueno porque tenemos la visualizaci�n redundante, 
#Ej. cor(mpg,cyl) es lo mismo que cor(cyl,mpg)
#Entonces solo nos interesa  la mitad de la matriz, esto lo podemos hacer mediante  la
#funci�n upper_tri: La funci�n se usa para devolver una matriz de valores l�gicos con el 
#tri�ngulo superior como VERDADERO.
#definamos unas funci�nes para sacar el "tr�angulo de arriba o de abajo

get_lower_tri<-function(x){
  x[upper.tri(x)] <- NA
  return(x)
}

get_upper_tri <- function(x){
  x[lower.tri(x)]<- NA
  return(x)
}

#Nos da igual trabajar con cualquiera de los dos tri�ngulos, pero hay 
#que seleccionar uno. na.rm eliminar� los NAs insertados en la funci�n

cor_mat_melted <- melt(get_upper_tri(cor_mat), na.rm = TRUE)


ggplot(data = cor_mat_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Valor Correlaci�n") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 2, barheight = 3,
                               title.position = "top", title.hjust = 0.5))+
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

#coord_fixed (): esta funci�n asegura que una unidad en el eje x 
#tenga la misma longitud que una unidad en el eje y.
#este gr�fico ya esta condensado, pero no existe un ordenamiento , ya que los ejes est�n
#ordenados en base a los nombre de  variables y no al valor 


####################################################
#DENDROGRAMAS
####################################################
#La medida de la distancia es una herramienta importante en el an�lisis
#estad�stico. Cuantifica la diferencia entre los datos de muestra para el
#c�lculo num�rico. 
#Una opci�n popular de m�trica de distancia es distancia euclideana , pero existen varias m�s: 
#"maximum", "manhattan", "canberra", "binary" , "minkowski" 
head(mtcars)
x <- mtcars["Mazda RX4 ",] 
y <- mtcars["Maserati Bora",] 

dist(rbind(x, y)) #Esto nos devuelve una matriz de distancias 

#Veamos un ejemplo sobre 4 veh�culos  
x <- mtcars["Mazda RX4 ",] 
y <-  mtcars["Mazda RX4 Wag",]  
a1 <- mtcars["Merc 280",]
a2 <- mtcars["Merc 280C",]
dist(rbind(x, y, a1,a2))

#Podemos observar que las distancias entre estos es muy peque�a
#Veamos las distancias de todo el dataset 
#Esta funci�n calcula y devuelve la matriz de distancia 
#calculada utilizando la medida de distancia especificada 
#para calcular las distancias entre las filas de una
#matriz de datos.

d <- dist(as.matrix(mtcars))   # obtenemos la matriz de distancias
d
#Un dendrograma es un diagrama que muestra la relaci�n jer�rquica 
#entre objetos. Por lo general, se crea como resultado de la agrupaci�n 
#jer�rquica. El uso principal de un dendrograma es encontrar la mejor
#manera de asignar objetos a grupos.


hc <- hclust(d)                # aplicamos agrupaci�n jer�rquica 
plot(hc)        

#Que estamos viendo en este diagrama: Por ejemplo que los Mazdas RX4 y Mazda RX$ Wag
#son muy similares. Igual el Merc 280 y Merc 280C


#La funci�n hclust usa el m�todo de enlace completo para 
#la agrupaci�n jer�rquica de forma predeterminada. 
#Este m�todo de agrupamiento en particular define la 
#distancia del grupo entre dos grupos como la distancia
#m�xima entre sus componentes individuales.

#Esta funci�n realiza un an�lisis de agrupamiento jer�rquico utilizando 
#un conjunto de diferencias para los  objetos que se agrupan. 
#Inicialmente, cada objeto se asigna a su propio cl�ster y luego el 
#algoritmo procede de forma iterativa, en cada etapa uniendo los dos 
#conglomerados m�s similares, continuando hasta que haya un solo cl�ster.



#Utilizando el concepto de distancias podemos ordenar nuestra
#diagrama de calor de acuerdo a los valores

reorder_cor_mat <- function(x){
  dd <- as.dist((1-x)/2)
  hc <- hclust(dd)
  x <-x[hc$order, hc$order]
}

#Reordenamos la matriz de correlaci�n 
cor_mat <-reorder_cor_mat(cor_mat)
#Obtenermos el tri�ngulo superior
upper_tri <- get_upper_tri(cor_mat)
# Transponemos esta matriz 
melted_cor_mat <- melt(get_upper_tri(cor_mat), na.rm = TRUE)
# Create a ggheatmap
ggplot(melted_cor_mat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Corelaci�n Ordenada") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

################################################
#DIAGRAMAS DE CONGLOMERADOS
################################################
#REDUCCION DE DIMENSIONES
#Uno de los �xitos de la visualizaci�n es poder agrupar la informaci�n 
#similar para que sea f�cil de visualizar relaciones. 
#Se desea buscar grupos dentro del conjunto de datos o la poblaci�n. 
#El desaf�o es que no siempre se sabe por d�nde empezar a mirar las 
#l�neas de conexi�n, por lo que ser�a bueno si se  puede  agrupar
#los objetos, seg�n varios criterios.

options(max.print=10000)
#Cargamos un dataset con informaci�n de ex�menes SAT
sats <-  read.csv("http://datasets.flowingdata.com/education.csv",
                  header=TRUE)
#revisemos la data 
names(sats)
head(sats)

#La primera pregunta es que tan diferentes son los estados en cuanto a los SATs
#La sintaxis de dist es :
#dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

#creamos la matriz de distancia  solo con las columnas de inter�s
state_dist <- dist(sats[,2:7])
#veamos la matriz de distancia 
state_dist
#Si desplegamos state_dist, veremos una matriz que nos indican 
#que tan separados est�n cada estados en relaci�n a otro
#(en realidad media matriz porque la distancia entre a y b igual a la dist ente b y a)

#Por ejemplo: si vemos el orden de los estados con : 
head(sats$state)

#Vemos que los dos primeros estados (eliminado todo los Estados Unidos)
#son Alabama y Alaska
#si vemos la matriz la intersecci�n de  fila 3 , col 2 es 86 , esta es la
#distancia relativa de los dos estados anteriores
length(state_dist)  #que es igual a 52*51/

#Dadas estas distancia haremos el ESCALADO MULTIDIMENSIONAL CLASICO  el cual  toma
#un conjunto de disimilitudes y devuelve un conjunto de puntos (sus coordenadas)
#de modo que las distancias entre los puntos son aproximadamente 
#iguales a las disimilitudes. 
#Esto lo hacemos mediante cmdscale()
#La sintaxis es:
#cmdscale(d, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE,
#list. = eig || add || x.ret)

sat_mds <- cmdscale(state_dist)
#este nos devuelve una matriz con las coordenadas de cada estado y  podemos visualizar
x <- sat_mds[,1]
y <- sat_mds[,2]
df<-data.frame(X=x, Y=y, S=sats$state)
ggplot(data=df, aes(x=X, y=Y))+ 
  geom_point()+
  geom_text(aes(label=S), size=2, fontface="bold", colour="navyblue",vjust=0, hjust=-0.1)  +
  ggtitle("Posicion relativa de los Estados por SAT")

#Ahora visualmente ya podemos ubicar clusters o conglomerados
#Es obvio que tenemos 2 o 3 grupos
#Pero si queremos verlo m�s visualmente usaremos : 

sat_cluster <- Mclust(sat_mds)
plot(sat_cluster)
#Opci�n 2 nos permite ver agrupaciones en conglomerados
#Opci�n 0 para salir 

#TAREA
#################################################################
#Vamos a hacer un an�lisis de clusterizaci�n de pa�ses de acuerdo 
#al impacto que el COVID ha tenido . 
#Para esto usaremos una estad�stica de OMS sobre el impacto a Juilio 2020

#Tomando como plantilla lo hecho anteriormente los pasos ser�an :
# Leer el archivo CSV a un data.frame
# Definir los campos de inter�s para el estudio para analisis de mortalidad
#  y para analisis de tasa de contagio 
# Crear la matriz de relaciones de distancia 
# Crear una jeraqu�a 
# Obtener el dataframe con los puntos representativos de cada pais
# Diagramar la agrupacion de conglomerados

#solucion 
#1.- read data 

#covid <- read.table("c:\\users\\alfonso\\Documents\\covit\\country_wise_latest.csv" , sep=",", header=TRUE, stringsAsFactors=FALSE)
covid19 <- read.table("https://covid19.who.int/WHO-COVID-19-global-data.csv" ,
                      sep=",", header=TRUE, stringsAsFactors=FALSE)

covid19 <- read.table("/Users/lgrocha/Dropbox/Cursos/Maestria/Visualizacion\ de\ datos/worldometer_data.csv" ,
                      sep=",", header=TRUE, stringsAsFactors=FALSE)
NROW(covid19)
head(covid19)
names(covid19)
View(covid19)

#2.- crear matriz de distancias

cov19=data.frame(Pais=NA, Casos=NA, TotalPruebas=NA, Muertes=NA)
cov19 <- data.frame(Pais = covid19$Country.Region, Casos = covid19$TotalCases,
                    TotalPruebas = covid19$TotalTests, Muertes=covid19$TotalDeaths)
cov19
covid19SinNA=na.omit(cov19)
covid19SinNA
#CASOS
cols<- c(2,3)
cols
rows<- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
rows
pairs(covid19SinNA[2:3], main = "Relacion de variables ",  bg = c("red", "green3", "blue"),  pch = 23)
###
#CALCULAMOS LA MATRIZ DE CORRELACION 

cor_matC <- round(cor(covid19SinNA[2:3]),2)
head(cor_matC)
#El formato de la matriz no aceptable para ggplot, vamos a transponer con la funci�n melt 

cor_mat_meltedC  <- melt(cor_matC)

#Ahora lo graficamos como  diagrama de calor 
#geom_tile sirve para hacer diagramas de calor en 2 dimensiones

ggplot(data = cor_mat_meltedC, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_viridis(discrete = FALSE, option = "A")
###
covid_dist <- dist(covid19SinNA[rows,cols])
covid_dist
#View(covid_dist)
head(covid_dist)
covid_mds <- cmdscale(covid_dist)

#3.- Diagramar puntos 
x <- covid_mds[,1]
y <- covid_mds[,2]
covid_mortality<-data.frame(X=x, Y=y, P=covid19SinNA$Pais)
ggplot(data=covid_mortality, aes(x=X, y=Y))+ 
  geom_point()+
  geom_text(aes(label=P), size=2, fontface="bold", colour="navyblue",vjust=0, hjust=-0.1)  +
  ggtitle("COVID-19 Contagios  por País")

#4.- Visualizar Clusters
covid_cluster <- Mclust(covid_mds)
plot(covid_cluster)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
covid19SinNA
#Mortalidad
cols<- c(2,4)
cols
rows<- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
rows
pairs(covid19SinNA[2:4], main = "Relacion de variables ",  bg = c("red", "green3", "blue"),  pch = 23)
###
#CALCULAMOS LA MATRIZ DE CORRELACION 

cor_matC <- round(cor(covid19SinNA[cols]),2)
head(cor_matC)
#El formato de la matriz no aceptable para ggplot, vamos a transponer con la funci�n melt 

cor_mat_meltedC  <- melt(cor_matC)

#Ahora lo graficamos como  diagrama de calor 
#geom_tile sirve para hacer diagramas de calor en 2 dimensiones

ggplot(data = cor_mat_meltedC, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_viridis(discrete = FALSE, option = "A")
###
covid_dist <- dist(covid19SinNA[rows,cols])
covid_dist
#View(covid_dist)
head(covid_dist)
covid_mds <- cmdscale(covid_dist)

#3.- Diagramar puntos 
x <- covid_mds[,1]
y <- covid_mds[,2]
covid_mortality<-data.frame(X=x, Y=y, P=covid19SinNA$Pais)
ggplot(data=covid_mortality, aes(x=X, y=Y))+ 
  geom_point()+
  geom_text(aes(label=P), size=2, fontface="bold", colour="darkred",vjust=0, hjust=-0.1)  +
  ggtitle("COVID-19 Mortalidad  por País")

#4.- Visualizar Clusters
covid_cluster <- Mclust(covid_mds)
plot(covid_cluster)



