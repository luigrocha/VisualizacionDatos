############################################################
#LABORATORIO DE  RELACIONES 1 
#En este laboratorio veremos 
#Diagramas mosaico 
#Diagramas de tamiz 
#Diagramas de asociaci�n
############################################################
#Cuando tratamos de visualizar relaciones, �stas pueden ser variables de tipo nominal o categ�ricas
#(factores) o variables de tipo num�rico. Este Laboratorio trata de variables de tipo categ�rico.
#El an�lisis de variables de tipo categ�rico se basa en tablas de contingencia. 
#Recordando el concepto de tablas de contingencia: :
#Una tabla de contingencia es una herramienta utilizada en la rama de la estad�stica,
#la cual consiste en crear al menos dos filas y dos columnas para representar datos 
#categ�ricos en t�rminos de conteos de frecuencia

#Un ejemplo de esta tabla puede ser :
#Variable categ�rica 1  Sexo  (M/F)
#Variable categ�rica 2  Titulo ( 2donivel , 3ernivel , 4tonivel )

rm(list=ls())
require(ggplot2)
require(datasets)
require(ggmosaic)
#install.packages("vcd")
require(vcd)
#install.packages("Epi")
require(Epi)
#install.packages("gtsummary")
require(gtsummary)
#install.packages("pubh")
require(pubh)
require(magrittr)
#install.packages("tidyr")
#install.packages("dplyr")
library("tidyr")
library("dplyr")
library("grid"); library("vcd")
#################################################
#DIAGRAMAS DE MOSAICOS 
################################################

data(HairEyeColor)
#View(HairEyeColor)
#Note que este dataset ya es una tabla de contingencia, si no tiene la tabla 
#sino las observaciones, usted la puede crear con  funcines como table() o cross_tab() 
hec <- HairEyeColor
hecdf <- as.data.frame(HairEyeColor) #convertimos en data.frame 
#hec y hecdf ya son tablas de contingencia 
#Solo para prop�sito  de demostraci�n  vamos a crear una tabla de contingencia de la 
#tabla de contingencia 
#Esto nos dar� una tabla de igual cantidad de observaciones 

dfreg <- as.data.frame(table(hecdf$Hair, hecdf$Eye))
names(dfreg) <- c("Hair", "Eye" , "Freq")
#View(dfreg)
ggplot(data=dfreg) +
  geom_mosaic(aes(weight=Freq, x=product(Hair, Eye)))+
  xlab("Eye") +
  ylab("Hair")

#Este mosaico nos estar�a indicando que no existe una relaci�n entre las variable Hair y Eye
#porque el numero de observaciones de pelo caf� - ojos cafes es la misma que pelo caf�- ojos verdes
#Obviamente esto es por la forma en que sacamos la tabla de contingencia 
#Ahora veamos la distribuci�n real 


ggplot(data=hecdf) +
  geom_mosaic(aes(weight=Freq, x=product(Hair, Eye)))+
  xlab("Eye") +
  ylab("Hair")

#Obviamente en este mosaico es claro que existe una relaci�n, no es lo mismo la ocurrencia de 
#pelo-caf�-ojos caf�s que pelo-cafe-ojos verdes

##PAQUETE VCD
#vcd significa visualize categorical  data, utilizado para visualizar contingencias multi-dimensionales
#integra t�cnicas como visualizaci�n de mosaicos, gr�ficos de asociaci�n y gr�ficos de tamices
#VCD almacena las tablas de contingencia en una estructura propia 
#structable produce una representaci�n "plana" de una tabla de contingencia de alta dimensi�n
#construida por divisiones recursivas (similar a la construcci�n de pantallas de mosaico

STD <- structable(~ Hair + Eye, data = HairEyeColor)

#La funci�n mosaic de vcd realiza el trabajo de obtener la tabla de contingencia y la visualizaci�n
#y puede trabajar con otras estructuras adem�s de data frames 

mosaic(~ Hair + Eye, data = STD)

mosaic(~ Hair + Eye, data = HairEyeColor)

#Un ejemplo interesante multi-dimensional es la table del Titanic. 
data(Titanic)
titanic <- as.data.frame(Titanic)
#Veamos las variables del data.frame 
names(titanic)
#Queremos  entender si existe una relaci�n entre la edad del pasajero y el 
#hecho que haya sobrevivido 

#Queremos entender si existe una relaci�n entre el Sexo y la Sobrevivi�
mosaic(~ Sex + Survived , data=titanic)
#Obviamente si existe una relaci�n 
mosaic(~ Age + Survived , data=titanic)
#El diagrama indica que hay una relaci�n , aunque no es fuerte, note que la cantidad de 
#ni�os que sobreviven es similar a la que no sobreviven. 
#Parece que el dicho de "Mujeres y ni�os primero" no se cumpli� m�s bien fue "Mujeres primero"

#Veamos un tercer ejemplo con  data de epidemiolog�a
data(Oncho)
#Entendamos el dataset
names(Oncho)  #este dataset tiene informaci�n sobre la enfermedad de oncocercosis en Sierra Leon 
#veamos que clase de objeto es 
class(Oncho)
#Cambiando a data.frame 
oncho <- as.data.frame(Oncho)
#View(oncho)
#Tratamos de entender que existe una relaci�n entre las personas infectadas en la selva vs sabana

mosaic(~mf +area , data=oncho)
#Podemos asegurar que existe una relaci�n.

#El ancho de cada columna de mosaicos  es proporcional a la frecuencia marginal de las variable
#Una vez m�s, el �rea de cada cuadro es proporcional a la frecuencia de la celda, y se
#muestra una independencia completa cuando todos los mosaicos de cada fila tienen la misma altura. 

#Cuando intentamos explicar los datos, asumimos la validez de un determinado modelo 
#para el proceso de generaci�n. En el caso de las tablas de contingencia bidireccionales, 
#las dos hip�tesis (H0) m�s �tiles son : 
#a) la independencia de las dos variables y
#b)  homogeneidad de una variable entre los estratos definidos por el segundo.

#La idea de realizar un gr�fico en base al modelo vs. realizar en base a las observaciones
#es evitar que el sesgo o ruido puedan impactar en las conclusiones 

#En este caso, los valores de la tabla de contingencia son remplazados por los  
#valores predichos (valores hat ) basados en una distribuci�n te�rica de 
#probabilidades ,en este caso binomial porque las variables son discretas.

## Tambi�n es posible visualizar mosaicos en base a los valores "esperados" en lugar de los 
#observados, es decir se ha desarrollado un modelo de predicci�n y en base a este se ven los
#valores esperados


mosaic(~ Hair + Eye, data = hec, type="expected")


###########################################################################
#  DIAGRAMAS DE TAMIZ 
###########################################################################
#Una vez que tenemos desarrollado un modelo, podemos hacer  una tabulaci�n cruzada de cada 
#mosaico para llenarlo con un n�mero de cuadrados igual al n�mero correspondiente de 
#frecuencias observadas, obtenemos un gr�fico de tamiz. 

#esto compara impl�citamente los valores esperados y observados, ya que la densidad de las 
#parcelas internas aumentar� con la desviaci�n de los valores observados de los esperados.
#esto permite la detecci�n de patrones de asociaci�n general (para variables nominales) 
#y de asociaci�n lineal (para variables ordinales), produciendo estas �ltimas parcelas de
#muy alta o muy baja densidad en una de las diagonales 

#La funci�n sieve traza rect�ngulos con �reas proporcionales a las frecuencias de celda 
#esperadas y rellenos con un n�mero de cuadrados igual a las frecuencias observadas.
#As�, las densidades visualizan las desviaciones de lo observado de los valores esperados. 

#Veamos un ejemplo  con HairEyeColor
sieve(~ Hair +Eye , data = hec, spacing = spacing_dimequal(c(0,0,0)))

#El argumento de spacing  permite separar  los casos para observar mejor 

sieve(~ Hair +Eye , data = hec, spacing = spacing_dimequal(c(2,2,2)))


#Que vemos en esta imagen: El �rea de cada rect�ngulo es proporcional a la frecuencia esperada
#y la frecuencia observada se muestra por el n�mero de cuadrados en cada rect�ngulo.
#Por lo tanto, la diferencia entre las frecuencias observadas y esperadas aparece 
#como la densidad del sombreado, utilizando el color para indicar si la desviaci�n de la 
#independencia es positiva o negativa. (En las versiones monocrom�ticas, los residuos positivos
#se muestran con l�neas continuas y los negativos con l�neas discontinuas). 

sieve(~ Hair +Eye , data = hec, shade=TRUE, main=TRUE, spacing = spacing_dimequal(c(2,2,2)))

#El par�metro shade permite  interpretar de mejor manera, cuando los valores esperados son 
#mayores que los observados se pintar� de rojo, caso contrario de azul 



######################################################
#DIAGRAMAS DE ASOCIACI�N
######################################################
#Alternativamente a los diagramas de tamiz, podemos inspeccionar directamente los residuos. 
#Los residuos de Pearson (desviaciones estandarizadas de los valores observados de los esperados) 
#se visualizan convenientemente usando la funci�n assoc().

assoc(hec, compress = FALSE) 

#Que estamos viendo: Hay una l�nea base, sobre la cual se dibuja una columna hacia arriba o hacia
# abajo dependiendo de la  desviaci�n entre los valores esperados y los valores observados. 
#En otras palabras, el diagrama de mosaico nos ha indicado si hay o no relaci�n entre 
#las variables (ejes) . Pero los mosaicos trabajan sobre los valores observados que pueden estar
#con sesgo y/o ruido. De esa independencia o relaci�n encontrada vamos contextualizar con 
#valores esperados del modelo . 







