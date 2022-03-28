rm(list=ls())

#library(usethis) 
#usethis::edit_r_environ()
#covid19 <- read.table("https://covid19.who.int/WHO-COVID-19-global-data.csv" ,
#                      sep=",", header=TRUE, stringsAsFactors=FALSE)
covid19 <- read.table("/Users/lgrocha/Downloads/WHO-COVID-19-global-data.csv" 
                    , sep=",", header=TRUE, stringsAsFactors=FALSE)

names(covid19)
NROW(covid19)
summary(covid19)
head(covid19)
covid19
library(dplyr)
#Estudio de mortalidad
#Country, Date_reported, NewDeaths, CumulativeDeaths
totalMortalidad=data.frame(Country=NA, totalCasos =NA,  totalMuertos =NA)
totalCovid=data.frame(Country=NA,totalCasos =NA,  totalMuertos =NA)
for (pais in unique(covid19$Country))
{
  totalCovid$totalCasos<- covid19 %>%   filter(Country == pais) %>%  summarise(sum(New_cases))
  totalCovid$totalMuertos<- covid19 %>%   filter(Country == pais) %>%  summarise(sum(New_deaths))
  totalCovid$Country<- pais
  names(totalCovid)<-c("Country", "totalCasos", "totalMuertos")
  totalMortalidad <-rbind(totalMortalidad ,totalCovid)
}
totalMortalidad
totalMortalidad<- na.omit(totalMortalidad)
totalMortalidad1=filter(totalMortalidad, totalMuertos>1)
totalMortalidad1
View(totalMortalidad1)
names(totalMortalidad1)
#Estudio sobre contagio
#Country, Date_reported, New_cases, Cumulative_cases
####################################################
#DENDROGRAMAS
####################################################
#La medida de la distancia es una herramienta importante en el analisis
#estadistico. Cuantifica la diferencia entre los datos de muestra para el
#calculo numerico. 
#Una opcion popular de metrica de distancia es distancia euclideana , pero existen varias mas: 
#"maximum", "manhattan", "canberra", "binary" , "minkowski" 


dcovid <- dist(as.matrix(totalMortalidad1))   # obtenemos la matriz de distancias
dcovid
View(totalMortalidad1)

cols<- c(1,3)
cols
#sessionInfo()
covid_dist <- dist(totalMortalidad1[,cols])
covid_dist

head(totalMortalidad1$Country)
length(covid_dist)

covid_mds <- cmdscale(covid_dist)


#este nos devuelve una matriz con las coordenadas de cada estado y  podemos visualizar
x <- covid_mds[,1]
y <- covid_mds[,2]
df<-data.frame(X=x, Y=y, S=totalMortalidad1$Country)
ggplot(data=df, aes(x=X, y=Y))+ 
  geom_point()+
  geom_text(aes(label=S), size=2, fontface="bold", colour="navyblue",vjust=0, hjust=-0.1)  +
  ggtitle("Posicion relativa de los Estados por SAT")


covid_cluster <- Mclust(covid_mds)
plot(covid_cluster)
cols<- c(2,3)
cols
cor_co19 <- round(cor(totalMortalidad1$totalCasos, totalMortalidad1$totalMuertos),2)
totalMortalidad1
summary(totalMortalidad1)
pairs(totalMortalidad1[2:3], main = "Relacion de variables ",  bg = c("red", "green3", "blue"),  pch = 23)
