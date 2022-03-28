#VISUALIZACION DE PROPORCIONES 
##################################################
#En este laboratorio veremos : 
#1.- DIAGRAMAS PASTEL Y COORDENADAS POLARES
#2.- MOSAICOS 
#3.- TREEMAPS
#4.- PARALEL SETS
##################################################
#A menudo queremos mostrar c�mo alg�n grupo, entidad o cantidad se descompone 
#en partes individuales que representan cada una  proporci�n del todo. 
#Los ejemplos comunes incluyen las proporciones de hombres y mujeres en un grupo de 
#personas, los porcentajes de personas que votan por diferentes partidos pol�ticos

#La visualizaci�n es el gr�fico t�pico es el pastel, aunque no muy usado en  la ciencia de datos. 
#Visualizar proporciones puede ser un desaf�o, en particular cuando el todo se divide
#en muchas partes diferentes o cuando queremos ver cambios en las proporciones a lo 
#largo del tiempo o entre condiciones.
#No existe una visualizaci�n ideal �nica que siempre funcione. 
rm(list=ls())
require(ggplot2)
#require(hrbrthemes)
install.packages("ggmosaic")
install.packages("dplyr")
require(dplyr)
require(tidyr)
require(viridis)
require(ggmosaic)
require(treemap)
require(tidyverse)
require(ggfittext)
require(treemapify)
require(plotrix)
require(ggforce)
require(reshape2)


################################################################
#DIAGRAMAS PASTEL Y COORDENADAS POLARES
################################################################
#No existe una geom particular para gr�ficos de pastel. Para esto usaremos 
#diagramas de barras y haremos la conversi�n a coordenadas polares
#coord_polar toma las siguientes argumentos: 
#Theta= variable a la que mapea el �ngulo 
#start= punto de inicio 
#direction = 1, clockwise; -1, anticlockwise
#Vamos a preparar un data frame con la poblaci�n de 5 estados
data(midwest)
names(midwest)
totales=data.frame( totalpop =NA,state=NA)
for (st in unique(midwest$state))
{
  statedf <- midwest %>%   filter(state == st) %>%  summarise( sum(poptotal))
  statedf$state <- st
  names(statedf)<-c("totalpop", "state")
  totales <-rbind(totales ,statedf)
} 
totales<- na.omit(totales)
totales$totalpop <- round(totales$totalpop/1000000, 2) # en millones 
View(totales)
#En coordenadas polares tenemos 2  variables que pueden ser mapeadas 
#al dataset . El radio r y el �ngulo theta.
#Si el �ngulo theta esta mapeado a la variable num�rica y no existe otro valor
#el gr�fico se convierte en un pastel cl�sico
ggplot(data=totales , aes(x="", y=totalpop , fill=state))+
  geom_bar(stat="identity") +coord_polar(theta="y", start=0)

#Note el uso de geom_bar

#Si existen 2 variables,  una categ�rica y otra num�rica, y el theta esta
#mapeado al valor num�rico obtenemos  y el radio a la categ�rica obtenemos un gr�fico tipo blanco 
#(target), entonces cada categ�rica tiene un radio definido, y los c�rculos podr�an no ser 
#completos. 
#Note los valores referenciales de la escala , 0,3,6,9 y compare con el dataset 
ggplot(data=totales , aes(x=state, y=totalpop , fill=state))+
  geom_bar(stat="identity") +coord_polar(theta="y", start=0)


#si no se especifica cual mapea a theta entonces el radio es mapeado
#a la num�rica y el �ngulo es constante .

ggplot(data=totales , aes(x=state, y=totalpop , fill=state))+
  geom_bar(stat="identity") +coord_polar( start=0)

#si no hay una segunda variable se obtendr� un blanco completo, en ese caso
#el radio es mapeado a la variable num�rica, pero cada factor se suma al anterior
ggplot(data=totales , aes(x="", y=totalpop , fill=state))+
  geom_bar(stat="identity") +coord_polar( start=0)

#Gr�ficos 3D
#ggplot no contiene librer�as para diagramas 3D, pero se hace usando esta
# la librer�a plotrix 

slices <- totales$totalpop
lbls <- paste(totales$state, as.character(totales$totalpop), "M")
pie3D(slices,labels=lbls,explode=0.1, main="Poblaci�n por estado Estado ")

#Los gr�ficos circulares tienen un caso de uso bastante reducido que se
#resume particularmente bien en su definici�n. 
#Su objetivo principal en un gr�fico circular debe ser comparar 
#la contribuci�n de cada grupo al todo, en lugar de comparar grupos 

#Vamos a crear una nueva columna alterando +- 10% el valor de la poblaci�n
totales$year <- "2000" 

for (st in totales$state)
{
  
  val <- totales[totales$state==st ,1]
  valmin<- val*.9
  valmax<- val*1.1
  val1 <- runif(1,valmin, valmax )
  df2010 <- data.frame(totalpop=round(val1) ,state=st, year="2010" )
  totales<-rbind(totales,df2010)
}

ggplot(data=totales , aes(x="", y=totalpop , fill=state))+
  geom_bar(stat="identity") +coord_polar( start=0)+
  facet_wrap(vars(year))

#En el ejemplo anterior tenemos 2 gr�ficos similares, de hecho, existen diferencias
#de  +- 10% y resulta muy dif�cil su comparaci�n


##########################################################
#MOSAICOS 
##########################################################
#Dise�ado para crear visualizaciones de datos categ�ricos, geom_mosaic()
#tiene la capacidad de producir gr�ficos de barras, gr�ficos de barras 
#apiladas, diagramas de mosaico y diagramas de dos pisos y por lo tanto, 
#ofrece una amplia gama de diagramas potenciales.
#Para usar ggmosaic dentro del marco de ggplot2, debemos ser capaces de 
#crear la f�rmula a partir de la est�tica (aes) definida . 
#Es decir, la est�tica configura la f�rmula que determina c�mo romper
#la distribuci�n conjunta. 
#La principal diferencia de geom_mosaic es que los gr�ficos de mosaico no 
#tienen un mapeo uno a uno entre una variable y el eje X o Y. 
#Para acomodar el n�mero variable de variables, la funci�n product ()
#crea el mapeo a X.  


data(Titanic)
titanic <- as.data.frame(Titanic)
titanic$Survived <- factor(titanic$Survived, levels=c("Yes", "No"))  #solo cambiamos a clase factor
head(titanic)
xlabel=paste("1st  ", "    2nd    " , "    3rd  " , "  Crew")
View(Titanic)

#Product es una funci�n para crear una lista que indica como debe  desglosar
#las partes, en este caso por Clase (1ra, 2da, 3ra,Crew) y el segundo factor si sobrevivi�
#Note que mosaic no dibuja valores en 0 
#Tambi�n note que mosaic no usa el argumento y, en su lugar habr� que usar weight 
#y este mapea al �rea
p1 <- ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Class), fill=Survived))+
  xlab(xlabel) +
  ylab("Sobrevive")
p1
#Como es dificil comparar �reas es recomendable colocar el valor correspondiente
#al �rea, el proceso es  un tanto complicado requiere manipular el objeto mismo (p1 en este caso) 

p1d<- ggplot_build(p1)$data %>% as.data.frame() %>% filter(.wt > 0)
p2<-p1 + 
  geom_label(data = p1d, aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2,  label = .n))
p2

#Podemos validar el gr�fico con un aggregate de la data
aggregate(data=titanic , .~Class+Survived ,sum)

#El siguiente gr�fico con 2 variables categ�ricas
p3 <- ggplot(data=titanic) + 
  geom_mosaic(aes(weight=Freq, x=product(Class,Age), fill=Survived))+
  theme(axis.text.x=element_text(angle=90, vjust=1))
p3
p3d<- ggplot_build(p3)$data %>% as.data.frame() %>% filter(.wt > 0)
p4<-p3 + 
  geom_label(data = p1d, aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2,  label = .n))
p4


#Validar las �reas con el siguiente aggregate
aggregate(data=titanic , .~Class+Age+Survived ,sum)

#Note que la correspondencia a una �rea en especial no es clara.
#Una forma de identificar las �reas mejor es mediante facets:

ggplot(data=titanic) + 
  geom_mosaic(aes(weight=Freq, x=product(Class,Age), fill=Survived))+
  labs(x="Edad", title='Sobrevivientes por Clase/Edad')  + 
  facet_grid(Class~.) + 
  guides(fill=guide_legend(title = "Survived", reverse = TRUE))+
  theme(axis.text.x=element_text(angle=90, vjust=1))

#Uno de los problemas de mosaicos es que pueden llevar a la conclusi�n que la suma de las
#partes es mayor que el total

#########################################################
#TREEMAPS
#########################################################
#Se usa un mapa de �rbol para "mostrar datos jer�rquicos
#usando figuras anidadas, generalmente rect�ngulos". 
#Hay muchas formas de hacerlo en  R, la primera es con la librer�a treemap y la segunda es con ggplot 
#Con la librer�a treemap se pueden lograr gr�ficos simples pero buenos en una fase exploratoria
#Los mapas de �rbol funcionan bien si las subdivisiones de un grupo son
#completamente distintas de las subdivisiones de otro


#geom_treemap tiene los siguientes aes: area (required),
#subgroup, subgroup2 , subgroup3 (required),colour,size,linetype,alpha

#Primero un poco de data wrangling porque la data no est� como la necesitamos
cars <- mtcars 
cars$carname <- rownames(cars)
cars <- mutate(cars, cyl = factor(cyl))

ggplot(data=cars, aes(area = disp, fill = cyl, label = carname)) +
  geom_treemap() +
  geom_treemap_text(
    fontface = "italic",
    colour = "white",
    place = "centre",
    grow = TRUE
  )

###############################################################
#PARALEL SETS
###############################################################
#En una visualizaci�n de conjuntos paralelos, a cada variable categ�rica se le 
#asignar� una posici�n en el eje x. 
#El tama�o de la intersecci�n de categor�as de variables vecinas se muestra luego 
#como diagonales gruesas, escaladas por la suma de elementos compartidos entre las 
#dos categor�as. La representaci�n de datos naturales para tal gr�fico es 
#tener cada variable categ�rica en una columna separada y luego tener una columna
#que indique la cantidad / magnitud de la combinaci�n de niveles en la fila. 
#Desafortunadamente, esta representaci�n no se ajusta a la API ggplot2, que necesita
#la codificaci�n de todas las posiciones en la misma columna.
#Para que sea m�s f�cil trabajar con ggforce, se proporciona un asistente 
#gather_set_data(), que se encarga de la transformaci�n. 


data(Titanic)
titanic<-as.data.frame(Titanic)

datadf <- reshape2::melt(Titanic)
head(datadf)
#Veamos un ejemplo con 2 niveles (Edad y Sobrevivencia)
data_gather <- gather_set_data(datadf, 3:4) #esta funci�n sirve para crear el set 
#1:4 son las columnas usadas como ejes, en ese orden, se puede alterar el orden de agreagaci�n 
#modificando  este orden 

ggplot(data_gather, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Sex), alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white')

##Veamos un ejemplo con 3 niveles (Edad, Sexo y sobrevivientes)

data_gather <- gather_set_data(datadf, c(3,2,4))
ggplot(data_gather, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Sex), alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white')


##Veamos un ejemplo con 4 niveles (Sexo, Edad clase y sobrevivientes)

data_gather <- gather_set_data(datadf, 1:4)
ggplot(data_gather, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Sex), alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white')



#Otras formas de sets paralelos pueden encontrar en este sitio: 
#https://www.data-imaginist.com/2019/the-ggforce-awakens-again/


###############################3



ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Class), fill=Survived))+
  geom_label(data=titanic, label = Freq)+
  xlab(xlabel) +
  ylab("Sobrevive")



p1<- ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Class), fill=Survived))



p1d<- ggplot_build(p1)$data %>% as.data.frame() %>% filter(.wt > 0)

compt_perc=function(x){
  d=c(x,1)-c(0,x)
  d[-length(d)] }

x=tapply(p1d$ymax,factor(p1d$fill,levels=unique(p1d$fill)),compt_perc)
x=unlist(x)
p1d$percentage=paste0(round(100*x,2),"%")
p2<-p1 + 
  geom_label(data = p1d, aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2,  label = .n))
p2<-p2+xlab("xlab")+ylab("ylab")+theme_bw()
p2<-p2+theme(legend.position="none")
p2