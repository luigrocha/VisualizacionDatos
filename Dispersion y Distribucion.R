#LAB DISPERSION Y DISTRIBUCION
##########################################################
#En este laboratorio veremos 
#1.-DIAGRAMAS DE PUNTOS
#2.-DIAGRAMAS DE CAJA BOXPLOTS
#3.-DIAGRAMA  DE VIOL�N 
#4.-HISTOGRAMAS 
#5.-DIAGRAMAS DE DENSIDAD 
#6.-POLIGONOS DE FRECUENCIA
#7.-DIAGRAMAS QQ
#########################################################

#Con frecuencia nos encontramos con situaciones en las que nos gustar�a comprender 
#c�mo se distribuye una variable en particular de un conjunto de datos. 
#Es fundamental entender la dispersi�n y distribuci�n de datos  a fin de poder aplicar
#adecuadamente los procesos estad�sticos. Por ejemplo si estoy tratando de inferir un 
#comportamiento especial  basado en el supuesto que dicho dataset tiene una distribuci�n 
#normal, pero el dataset no tiene dicha distribuci�n, cualquier inferencia ser� equivocada
#Otra situaci�n com�n es tratar de definir si dos muestras corresponden a la misma poblaci�n.
#########################################################
#DIAGRAMAS DE PUNTOS
#########################################################
#En la forma m�s simple podemos evaluar la dispersi�n mediante un diagrama  puntos (scatter)
rm(list=ls())
require(dplyr)
require(ggplot2)
require(ggplot2movies)
require(openintro)
require(e1071)
require(viridis)
require(cowplot)
data(mtcars)
head(mtcars)
ggplot(mtcars, aes(x=wt,y=mpg)) + geom_point()   #geom_point 
#Este diagrama nos muestra la dispersi�n de la data, sin embargo no nos dice mucho, 
#tenemos que evaluar si la dispersi�n  es igual con respecto a distintos factores 
#para esto utilizaremos el atributo colour o shape para crear grupos con cada nivel del factor
ggplot(mtcars, aes(mpg, x=wt, y=mpg, shape = factor(cyl))) +
  geom_point(aes(colour = factor(cyl)), size = 3) 
#Notamos con este gr�fico que la data mpg no esta  distribuida uniformemente entre 10 y 35 y que el 
#factor cyl  influye sobre el rango 
#Nos interesa saber como est� dispersa  el consumo de gasolina en carretera vs. los cyl 
data(mpg)
head(mpg)
ggplot(mpg, aes(x=cyl, y=hwy)) +
  geom_point(aes(colour=cyl) )
#Este gr�fico nos indica el rango de la dispersi�n, sin embargo, no nos indica la distribuci�n
#exacta porque los puntos se sobre dibujan , para solucionar el problema usaremos geom_jitter
ggplot(mpg, aes(x=cyl, y=hwy)) +
  geom_point(aes(colour=cyl) )+
  geom_jitter()
#Ok, con este gr�fico ya tenemos una visi�n de como se distribuyen, pero obtener estad�sticos
#exactos como la media no es f�cil con este gr�fico . 
#Para esto usaremos boxplots
##########################################
#DIAGRAMAS DE CAJA BOXPLOTS
##########################################
#Un boxplot es un m�todo para representar gr�ficamente grupos de datos num�ricos a 
#trav�s de sus cuartiles. Los diagramas de caja tambi�n pueden tener l�neas que se 
#extienden desde los l�mites llamados bigotes que indican variabilidad fuera de los
#cuartiles superior e inferior.
#Los siguientes aes son reconocidos
#x, y, lower o xlower,upper o xupper,middle or xmiddle,ymin o xmin,ymax o xmax
#alpha,colour,fill,group,linetype,shape,size, weight

ggplot(data=mpg, aes(y=hwy, x=cyl , group=factor(cyl), fill=factor(cyl))) +
  geom_boxplot() +
  scale_fill_discrete()   #scale_fill_discrete es para darle color a cada grupo de cyl

#Ahora ya es f�cil encontrar la media, el valor �nter cuart�lico IQR  y los 1ro y 3er 
#cuartiles y podemos ver que ninguno de los factores nos da una distribuci�n normal

#Es importante mencionar que si nos interesa ver una sola variable (sin comparaciones por factor)
#podemos colocarlo as�, de esa manera el eje x esta centrado en 1 
ggplot(data=mpg, aes(y=hwy, x=1)) +
  geom_boxplot(color="blue")
#Boxplots tambi�n nos permite visualizar los outliers 
ggplot(data=mpg, aes(y=hwy, x=1)) +
  geom_boxplot(color="blue",outlier.colour="palevioletred3", outlier.shape = 8)

#Note que tenemos 2 estrellas indicando outliers, sin embargo esto es enga�oso porque m�ltiples 
#outliers  pueden estar  en el mismo valor y no ser visibles 
#Para visualizar esto usaremos geom_jitter

ggplot(data=mpg, aes(y=hwy, x=1)) +
  geom_boxplot(color="blue",outlier.colour="palevioletred3", outlier.shape = 8)+ 
  geom_jitter() 

#Por �ltimo, el plot est� bien, pero es dif�cil visualizar los valores de min,max etc.

tab <- summary(mpg$hwy)
ggplot(data=mpg, aes(y=hwy, x=1)) +
  geom_boxplot(color="blue",outlier.colour="palevioletred3", outlier.shape = 8)+
  annotate("text", x=1, y =tab[1], label = as.character(tab[1])   )+
  annotate("text", x=1, y =tab[2], label = as.character(tab[2])   )+
  annotate("text", x=1, y =tab[4], label = as.character(round(tab[4]))   )+
  annotate("text", x=1, y =tab[5], label = as.character(tab[5])   )+
  annotate("text", x=1, y =tab[6], label = as.character(tab[6])   )



###################################################
#DIAGRAMA  DE VIOL�N 
###################################################
#Los diagramas viol�n son igual que los boxplots pero  incluyen una 
#distribuci�n  al costado , es decir no solo interesa saber
#cual es el valor de  1q, media , 3q, etc.  sino que dentro del Q1
#queremos saber cual es la distribuci�n, tiene un sesgo (skew) o kurtosis?.
#Lo mismo veremos en otros gr�ficos pero ahora usaremos viol�n

#En primer lugar veremos un viol�n de una distribuci�n est�ndar 
rn<- data.frame(X=rnorm(10000, 10,20 ))
ggplot(data=rn , aes(x=1 , y=X))+ geom_violin(color="blue")


ggplot(data=mpg, aes(y=hwy, x=1)) +
  geom_violin(color="blue") 

#El gr�fico no se parece a un viol�n m�s bien parece tener una distribuci�n bimodal 
#esto indicar�a que ya sea la data esta as� o que existe una combinaci�n de factores
#comprobemos esto �ltimo agrupando por cyl
ggplot(data=mpg, aes(y=hwy, x=cyl , group=factor(cyl), fill=factor(cyl)) ) +
  geom_violin() +
  scale_fill_discrete() 

#El dataset de mpg no tiene tantas observaciones por lo tanto el viol�n se nota un poco
#aplanado, adem�s seguimos teniendo outliers en la gama de 4 cyl, en 8 cyl se ve mas claro
#Vamos a repetir con un dataset m�s grande 
ggplot (diamonds , aes(y=carat ,x=cut, fill=cut)) + geom_violin()
#Que nos indica este gr�fico? Que ninguno de los subsets tienen distribuci�n est�ndar.
#Para comparaci�n veamos como se ve un viol�n con un distribuci�n normal


#Por defecto los boxplots y violins calculan la distribuci�n en cuartiles
#sin embargo es posible indicar percentiles 
ggplot(diamonds , aes(y=carat ,fill=cut ,x=cut)) +
  geom_violin(draw_quantiles=c(.1,.5, .75))+geom_point(size=1)+
  scale_fill_brewer(palette="Pastel2")

#Que conclusiones sacamos de este diagrama: 
#1.- La mayor�a de los diamantes se hallan en el rango de menos de 1 kilate 
#2.- Como el precio depende del corte, los diamantes m�s caros(ideal) tienden a ser m�s peque�os
#3.- Existen muchos de mal corte que son excepcionalmente grandes

###################################################
#HISTOGRAMAS
###################################################
#Los histogramas son gr�ficos que indican la frecuencia con la que ocurre ciertos valores del dataset.
#Los histogramas no se pueden elaborar con atributos, sino con variables medibles tales como peso, temperatura, tiempo, etc.
#La distribuci�n es contenida en columnas (bins) cuyo "ancho de intervalo" (binwidth) es programable
#Debido a esto, su apariencia visual exacta depende de la elecci�n del ancho del intervalo.
#La mayor�a de los programas de visualizaci�n que generan histogramas elegir�n un ancho 
#de intervalo de forma predeterminada, pero es probable que el ancho de intervalo 
#deba ser cambiado. Por lo tanto, es fundamental probar siempre diferentes anchos que refleje los
#datos subyacentes con precisi�n. 


data(diamonds)
names(diamonds)
#Nos preguntamos cual es la distribuci�n de la variable carat?
ggplot(diamonds, aes(x=carat)) + geom_histogram()
#El escoger el ancho del bin tiene que ver con la variable. En este caso 
#como estamos visualizando los carat(quilates) y que se miden en gramos . Escogemos .1
ggplot(diamonds, aes(carat)) + geom_histogram(binwidth = 0.1)
#Alternativamente podemos definir una cierta cantidad de bins
ggplot(diamonds, aes(carat)) +
  geom_histogram(bins = 200) 
#El escoger el n�mero de bins es clave, si usted escoge un valor bajo el gr�fico saldr� como escalones
#el escoger un valor alto presentar� una gran cantidad de l�neas finas que har� que se pierda 
#la distribuci�n general . Veamos
ggplot(diamonds, aes(carat)) +
  geom_histogram(bins = 100)
ggplot(diamonds, aes(carat)) +
  geom_histogram(bins = 25)

# Podemos  cambiar el eje  a Y para  obtener un gr�fico  horizontal 
ggplot(diamonds, aes(y = carat)) +
  geom_histogram(binwidth=.5)

#En general, si el ancho del contenedor es demasiado peque�o, entonces el histograma
#tendr� demasiados picos y se ver� visualmente atestado y las principales tendencias 
#en los datos no son visualizables Por otro lado, si el ancho del contenedor es demasiado
#grande,las caracter�sticas m�s peque�as en la distribuci�n de los datos, como la ca�da
#alrededor de los 0.8 quilates desaparece. 

#Si queremos separar el gr�fico entre bins y colocar una marca  podr�amos usar 
#geom_bar  y  `scale_x_binned`. 
#Esto es posible ya que un histograma no es m�s que un diagrama de barras cuya cuenta
#de registros se hace por binwidth

#scale_x_binned () y scale_y_binned () son escalas que discretizan datos de posici�n continuos.
#Puede usar estas escalas para transformar entradas continuas antes de usarlas con una geom
#que requiere posiciones discretas 

ggplot(diamonds, aes(carat)) +
  geom_bar() + scale_x_binned()

#Como conclusi�n, histogramas y barras son similares, la parametrizaci�n permite presentar la informaci�n
#en forma m�s entendible al usuario 
########################################################
#HISTOGRAMAS APLILADOS
########################################################
#Si el dataset tiene otros factores, podemos ver la distribucion por cada factor 
ggplot(diamonds, aes(price, fill = cut)) + geom_histogram(binwidth = 500)
#Este tipo de histograma no es muy util para visualizar distribuci�n con muchos factores,
#adicionalmente no es claro el alto de las barras.

ggplot(diamonds, aes(price, fill = cut)) +
  geom_histogram(aes(alpha=.20),binwidth = 500)+
  scale_fill_brewer(palette="Set2")
#Usando el par�metro alpha de  opacidad podemos ver mejor los 2 histogramas, pero  
#no funciona adecuadamente 
####################################################
#POLIGONOS DE FRECUENCIA
####################################################
#Un pol�gono de frecuencias se forma uniendo los extremos de las barras de un diagrama de barras
#mediante segmentos. Tambi�n se puede realizar trazando los puntos que representan las frecuencias
#y uni�ndolos mediante segmentos.

#Los pol�gonos de frecuencia (geom_freqpoly ()) son m�s adecuados cuando desea comparar la 
#distribuci�n entre los niveles de una variable categ�rica o factor. 
ggplot(diamonds, aes(price, colour = cut)) +
  geom_freqpoly(binwidth = 500)

#El gr�fico anterior ya permite una buena comparaci�n, pero el eje Y muestra la cuenta 
#Pero si comparamos sets con distinta cantidad de registros este gr�fico no nos sirve
#Para facilitar la comparaci�n de distribuciones con recuentos muy diferentes, 
# usaremos  la densidad en el eje Y en lugar del count() que es predeterminado

#Para entender after_stat debemos conocer que : ggplot2 tiene tres etapas de datos desde las que
#puede mapear la est�tica. En forma predeterminada se mapea al principio, utilizando los datos
#del dataset. La segunda etapa es despu�s de que la capa de stat haya calculado los valores, esto 
#lo hacer el after_stat.
#La tercera  etapa es despu�s de que los datos hayan sido transformados y mapeados por las escalas, 
#esto se indica con after_scale

#En el siguiente ejemplo utilizaremos after_stat

ggplot(diamonds, aes(price, after_stat(density), colour = cut)) +
  geom_freqpoly(binwidth = 500)

#Ahora las densidades si son comparables
########################################################
#PLOTS DE DENSIDAD y DENSIDADES APILADAS
########################################################
#Si solo queremos ver la densidad de una variable usaremos geom_density 
#ejemplo, la densidad de la columna carat
data(diamonds)
ggplot(diamonds, aes(x=carat)) +
  geom_density(color="blue")
#Lo dicho anteriormente, nos interesa ver la tendencia, con un tama�o de bin muy peque�o 
#la traza se vuelve con muchos picos y evita ver la tendencia. 
#El par�metro adjust permite ajustar de ancho del bin. 
#Esto hace posible ajustar el ancho de bin sin dejar de usar el estimador de ancho
#Por ejemplo, ajustar = 2 significa usar el doble del ancho de bin  
#predeterminado


g3<-ggplot(diamonds, aes(carat)) +
  geom_density(adjust = 3,  color="blue") +ggtitle("Adjust=3")
g4 <- ggplot(diamonds, aes(carat)) +
  geom_density(adjust = 4, color="blue")+ggtitle("Adjust=4")
g5 <- ggplot(diamonds, aes(carat)) +
  geom_density(adjust = 5, color="blue")+ggtitle("Adjust=5")

plot_grid(g3,g4,g5)

#Con el gr�fico anterior ya podemos ver la densidad de todo el dataset 
#comparando densidades por cada factor, centr�ndose en la tendencia y no en el detalle
#Ahora lo hacemos por calidad de corte (cut) 
ggplot(diamonds, aes(price, fill = cut, colour = cut)) +
  geom_density() 

#Pero no es claro porque se sobrelapan lo colores. Veamos un alpha
ggplot(diamonds, aes(price, fill = cut, colour = cut)) +
  geom_density(alpha=0.1) 



#Distribuciones que tienen alto skew 
#En este gr�fico podemos ver que la distribuci�n de price esta altamente 
#corrida  la izquierda  nos da un n�mero positivo alto. Podemos revisar con la f�rmula en 
#https://www.rdocumentation.org/packages/e1071/versions/1.7-7/topics/skewness

skewness(diamonds$price)
#Hay veces que la data esta  distribuida en un rango muy grande, podemos verle 
#mejor si aplicamos una transformaci�n del eje x

ggplot(data = diamonds, aes(price, color= cut , alpha=.5)) + 
  geom_density(fill="lightblue", adjust=4)+
  scale_x_continuous(trans="log10")

####################################
#DIAGRAMAS QQ
####################################
#Los gr�ficos de cuantiles-cuantiles (q-q) son una visualizaci�n �til cuando queremos
#determinar en qu� medida los puntos de datos observados siguen o no una distribuci�n 
#determinada. Sin embargo, en los gr�ficos q-q no graficamos los rangos directamente, 
#los usamos para predecir d�nde deber�a caer un punto de datos dado si los datos
#se distribuyen de acuerdo con una distribuci�n de referencia espec�fica. 
#Por lo general, las gr�ficas q-q se construyen usando una distribuci�n normal 
#como referencia.
#geom_qq usa los siguientes aes:
#sample,group,x, y 
#x corresponde a los cuantiles te�ricos (referencia)

vect <-rt(50, df=8)
sampledf <-data.frame(x=vect) #un dataframe con distribuci�n t
df <- data.frame(x=rnorm(50))  #un dataframe con distribuci�n normal

ggplot(data=df, aes(sample=sampledf$x))+
  geom_qq()+ stat_qq_line()

#o con la siguiente sintaxis, es igual, ya que geom_qq usa stat_qq
#stat_qq_line  dibuja la linea de referencia
ggplot(df, aes(sample = sampledf$x), color = "blue") +
  stat_qq() + 
  stat_qq_line()

#Que conclusi�n sacamos? Bueno que los dos sets no tiene la misma distribuci�n 
#si la tuvieran estar�an alineados en la recta

#Confirmemos lo visto anteriormente que la variable mpg  no est� distribuida normalmente
ggplot(mtcars, aes(sample = mpg)) +
  stat_qq() + stat_qq_line()


#Similar pero con  trazas por cada cyl 
ggplot(mtcars, aes(sample = mpg, colour = factor(cyl))) +
  stat_qq() + stat_qq_line()

