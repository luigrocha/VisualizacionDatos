#solucion 
#1.- read data 
require(viridis)
require(ggplot2)
require(reshape2)
require(viridisLite)
require(mclust)
require(knitr)
#covid <- read.table("c:\\users\\alfonso\\Documents\\covit\\country_wise_latest.csv" , sep=",", header=TRUE, stringsAsFactors=FALSE)
covid19 <- read.table("https://covid19.who.int/WHO-COVID-19-global-data.csv" ,
                      sep=",", header=TRUE, stringsAsFactors=FALSE)

covid19 <- read.table("/Users/lgrocha/Dropbox/Cursos/Maestria/Visualizacion\ de\ datos/worldometer_data.csv",
                      sep=",", header=TRUE, stringsAsFactors=FALSE)
NROW(covid19)
head(covid19)
names(covid19)
#View(covid19)

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

###
covidPuntos <- covid19SinNA[rows,]
covidPuntos
#View(covid_dist)
head(covid_dist)
covid_mds <- cmdscale(covid_dist)

#3.- Diagramar puntos 
x <- covid_mds[,1]
y <- covid_mds[,2]
covid_mortality<-data.frame(X=x, Y=y, P=covidPuntos$Pais)
ggplot(data=covid_mortality, aes(x=X, y=Y))+ 
  geom_point()+
  geom_text(aes(label=P), size=2, fontface="bold", colour="navyblue",vjust=0, hjust=-0.1)  +
  ggtitle("COVID-19 Contagios  por País")

#4.- Visualizar Clusters
covid_cluster <- Mclust(covid_mds)
plot(covid_cluster)
####################################################3
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

covidPuntos <- covid19SinNA[rows,]
covidPuntos
#View(covid_dist)
head(covid_dist)
covid_mds <- cmdscale(covid_dist)

#3.- Diagramar puntos 
x <- covid_mds[,1]
y <- covid_mds[,2]
covid_mortality<-data.frame(X=x, Y=y, P=covidPuntos$Pais)
ggplot(data=covid_mortality, aes(x=X, y=Y))+ 
  geom_point()+
  geom_text(aes(label=P), size=2, fontface="bold", colour="darkred",vjust=0, hjust=-0.1)  +
  ggtitle("COVID-19 Mortalidad  por País")

#4.- Visualizar Clusters
covid_cluster <- Mclust(covid_mds)
plot(covid_cluster)


#2.- De la información provista en el archivo desarrolle
#un diagrama barras apiladas de los casos por año por región usando la librería NVD3
#https://covid19.who.int/WHO-COVID-19-global-data.csv

head(covid19)


c1<- nPlot(TotalCases ~ Country.Region, group = 'Continent',
           data = subset(covid19),
           type = 'multiBarChart')
c1
