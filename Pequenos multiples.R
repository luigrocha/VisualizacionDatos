#PEQUE�OS M�LTIPLES 
######################################################
#En este laboratorio veremos
#Facet_grid
#Facet_wrap
#######################################################
#Conocidos tambi�n como diagramas Trellis o "faceting"   la idea clave es dividir 
#los datos en partes de acuerdo con una o m�s dimensiones de datos, visualizar 
#cada segmento de datos por separado y luego organizar las visualizaciones
#individuales en una cuadr�cula. Las columnas, filas o paneles individuales
#de la cuadr�cula est�n etiquetados por los valores de las dimensiones de los 
#datos que definen los segmentos de datos
#################################################################
#FACET_GRID 
#################################################################
#Facet grid es un opcion de ggplot2 en la cual los gr�ficos estan organizados en una cuadr�cula.
#Eso resulta ser muy �til para la producci�n de un gr�fico por cada par de variable discreta; 
#o sea, cada grupo producir� un gr�fico de acuerdo a su categor�a. 

#La sintaxis de la sentencia es : 
#facet_grid(  rows = NULL,  cols = NULL,  scales = "fixed | free",  space = "fixed",  shrink = TRUE,
#labeller = "label_value",  as.table = TRUE,  switch = NULL, drop = TRUE,   margins = FALSE,
#facets = NULL )

rm(list=ls())
require(ggplot2)
data(mpg)
names(mpg)
#Veamos un ejemplo sobre la base mpg,en el cual las  filas = tipo de transmisi�n ( 3 nivele) y
#col=# cyl ( 4 nivels)
#en otras palabras dividiremos la data en 12 partes de acuerdo a las variable y
#graficamos el consumo en ciudad (cty) en funci�n de la capacidad del motor(disp)
# La funci�n vars es una forma de llamar a select de dplyr
ggplot(mpg, aes(x=displ, y=cty)) + 
  geom_point()+
  facet_grid(rows = vars(drv))+
  facet_grid(cols = vars(cyl))+
  facet_grid(vars(drv), vars(cyl))

#Muchas veces nos interesa comparar  cada grupo con respecto a un valor,por ejemplo 
#la media de todos los grupos. Esto lo podemos hacer con trazado de lineas horizontales o 
#verticales dependiendo de la  distribuci�n del gr�fico

f <- data.frame(displ = mean(mpg$displ), cty = mean(mpg$cty))
ggplot(mpg, aes(displ, cty)) + 
  geom_point()+
  facet_grid(rows = vars(cyl)) +
  geom_hline(yintercept=f$cty , color="red")

#En general debemos tratar de que las escalas de todos los gr�ficos sean iguales 
#Esto facilita la comparaci�n , sin embargo, es posible que no todas los rangos  de valores 
#coincidan, por ejemplo para un grupo son muy grandes y para otro grupo muy peque�as 
#y para los grupos no es la misma. En ese caso pueden aparece  grandes
#espacios vac�os. 
#Para evitar esto podemos usar el argumento scales (fixed o free)
#En este caso obviamente el consumo de gas es  diferente para cada grupo de cyl 
ggplot(data=mpg, aes(x=displ, y=hwy, colour = factor(cyl))) +
  geom_point()+
  facet_grid(vars(cyl), scales = "free")



#Otra forma de indicar la relaci�n de filas y columnas a un facet_grid es mediante una f�rmula
# ~ se lee asi: "Como distribuye de acuerdo a cyl"
ggplot(data=mpg, aes(x=displ, y=hwy, colour = factor(cyl))) +
  geom_point()+
  facet_grid(~ cyl, scales = "free")

#Si hay mas de 2 variables tipo factor 
ggplot(data=mpg, aes(x=displ, y=hwy, colour = factor(cyl))) +
  geom_point()+
  facet_grid(~ cyl + drv , scales = "free")



#Cuando tenemos variable discretas es importante presentar el valor de la variable
#al margen, esto lo hacemos con el argumento margins.
#Los m�rgenes son facetas adicionales que contienen todos los datos para cada uno
#de los posibles valores de las variables de facetas. Si es FALSE, no se incluyen
#facetas adicionales (el valor predeterminado).
#Si es TRUE, los m�rgenes se incluyen para todas las variables de facetado. 


ggplot(data=mpg, aes(x=displ, y=hwy, colour = factor(cyl))) +
  geom_point()+
  facet_grid(~ cyl + drv , margins= FALSE, scales = "free")



#Como indicamos en la parte te�rica, no hace falta etiquetar las partes, pero de ser necesario
#puede recurrir a labeller. 


ggplot(data=mpg, aes(x=displ, y=hwy, colour = factor(cyl))) +
  geom_point()+
  facet_grid(~ drv ,  scales = "free")


#Pero tambi�n podemos poner un t�tulo m�s explicativo 
#Pero el valor de 4 no es nombre de variable v�lida, tenemos que cambiarlo en este caso 
mpg[mpg$drv == "4x4" ,7] = "x4"

transmision <- c(
  f = "Delantera",
  r = "Trasera",
  x4 ="4X4",
  cyl="cilindros"
)


ggplot(data=mpg, aes(x=displ, y=hwy, colour = factor(cyl))) +
  geom_point()+
  facet_grid(~ drv , labeller = labeller( drv = transmision))




#################################################################
#FACET_WRAPS 
#################################################################
#Vs y am tienen 2 posibles valores cada uno, y gear 3 posibles valores, 
#adem�s facet_grid realiza un gr�fico adicional con todos los valores 
#de cada variable, por lo tanto este c�digo presenta 3*3*3 +1 = 28 m�ltiples,
#aun cuando algunos  est�n vac�os. Facet_wrap evita dibujar esto bloques vac�os
#Comparemos las dos visualizaciones siguientes
ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  facet_grid(vars(vs,am,gear))+
  ggtitle("Facet_grid, Horrible Gr�fico")

#Este gr�fico ya esta mejor,pero  los t�tulos de cada columna no son entendibles
ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  facet_wrap(vars(vs,am,gear))+
  ggtitle("Facet_wrap,  pero t�tulos no entendibles")

# Uso de  `labeller` para control de como se imprimen etiquetas
ggplot(mpg, aes(displ, hwy, colour=class)) +
  geom_point() +
  facet_wrap(vars(cyl, drv), labeller = "label_both")+
  ggtitle("Facet_wrap,  Etiquetas = Nombres de columna")


ggplot(mpg, aes(displ, hwy, colour=class)) +
  geom_point() +
  facet_wrap(vars(cyl, drv), labeller = labeller( drv = transmision))+
  ggtitle("Mejor ")



# Dependiendo de donde se publicar� la informaci�n es posible que desee definir 
#la cantidad de filas, para que el gr�fico no sea tan ancho 
ggplot(mpg, aes(displ, hwy, colour=class)) + geom_point()+
  facet_wrap(vars(class), nrow = 4)

# Para cambiar el orden en el que aparecen los paneles, cambiar los niveles
# del factor subyacente.
mpg$class2 <- reorder(x=mpg$class,X=mpg$hwy)

ggplot(mpg, aes(displ, hwy, colour=class)) +
  geom_point() +
  facet_wrap(vars(class2))

