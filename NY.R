rm(list=ls())
require(coefplot)
require(ggplot2)
#require(hrbrthemes)
#install.packages("dplyr")
require(dplyr)
require(tidyr)
require(viridis)
require(ggmosaic)
require(treemap)
install.packages("treemap")
require(tidyverse)
require(ggfittext)
require(treemapify)
require(plotrix)
require(ggforce)
require(reshape2)

housing <- read.table("https://www.jaredlander.com/data/housing.csv" ,
                      sep=",", header=TRUE, stringsAsFactors=FALSE)
names(housing)
housing
#Nombre largos o con puntos es mejor cambiarlos 
nombres <- names(housing) 
names(housing) <- c("sector" ,"class", "TUnits" , "Year","GSqFt" ,
                    "Est_GIncome" ,"GIncomexSqFt","Est_Expense" , 
                    "ExpSqFt","NetIncome",   
                    "FullValue" , "valxSqFt" ,"Boro")
#Entendamos el dataset
head(housing)
unique(housing$Boro)
table(housing$Boro)
summary(housing)
################################################################
#DIAGRAMAS PASTEL Y COORDENADAS POLARES
################################################################
pie(table(housing$class))
pie(table(housing$sector))

spineplot(housing)



#1
total=data.frame( totalClass =NA,class=NA)
for (clss in unique(housing$class))
{
  classdf <- housing %>%   filter(class == clss) %>%  summarise(sum(TUnits))
  classdf$class <- clss
  names(classdf)<-c("totalClass", "class")
  total <-rbind(total ,classdf)
}
total<- na.omit(total)
total$totalClass <- round(total$totalClass/1000, 2) # en millones 
View(total)

ggplot(data=total , aes(x="", y=totalClass , fill=class))+
  scale_fill_brewer(palette = "Pastel3") +
  geom_label_repel(data = total,
                   aes(y = totalClass, label = paste0(totalClass, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Grupo")) +
  geom_bar(stat="identity") +coord_polar(theta="y", start=0)+
  ggtitle("Porcentaje por Clase")



total1=data.frame( totalSector =NA,sector=NA)
for (clss in unique(housing$sector))
{
  nedf <- housing %>%   filter(sector == clss) %>%  summarise(sum(TUnits))
  nedf$Neighborhood <- clss
  names(nedf)<-c("totalSector", "sector")
  total1 <-rbind(total1 ,nedf)
}
total1<- na.omit(total1)
total1$totalSector <- round(total1$totalSector/1000, 2) # en millones 
View(total1)

ggplot(data=total1 , aes(x="", y=totalSector , fill=sector))+
  geom_bar(stat="identity") +coord_polar(theta="y", start=0)

total2=data.frame( totalYear =NA, Year=NA)
for (clss in unique(housing$Year))
{
  YearBuiltdf <- housing %>%   filter(Year == clss) %>%  summarise(sum(TUnits))
  YearBuiltdf$Year  <- clss
  names(YearBuiltdf)<-c("totalYear", "Year")
  total2 <-rbind(total2 ,YearBuiltdf)
}
total2<- na.omit(total2)
total2$totalYear <- round(total2$totalYear/1000, 2) # en millones 
View(total2)


ggplot(data=total2 , aes(x="", y=totalYear , fill=Year))+
  geom_bar(stat="identity") +coord_polar(theta="y", start=0)

#LAB DISPERSION Y DISTRIBUCION
##########################################################
#En este laboratorio veremos 
#1.-DIAGRAMAS DE PUNTOS
#2
housing80=filter(housing, Year>=1980)
View(housing80)
ggplot(housing80, aes(x=housing80$Year,y=GSqFt)) + geom_point()   #geom_point 

ggplot(housing80, aes(GSqFt, x=Year, y=GSqFt, shape = factor(class))) +
  geom_point(aes(colour = factor(class)), size = 3) 


#3.

ggplot(housing, aes(x=Year,y=GSqFt, color = "darkblue")) + geom_point()   #geom_point 


ggplot(data=housing, aes(y=GSqFt, x=Year , group=factor(class), fill=factor(class))) +
  geom_boxplot() +
  scale_fill_discrete() 

ggplot(data=housing, aes(y=GSqFt, x=Year , group=factor(Boro), fill=factor(Boro))) +
  geom_boxplot() +
  scale_fill_discrete() 
#LAB-COORDENADAS_GEODESICAS
#############################################################

# 4.1 

unique(housing$Boro)
table(housing$Boro)
housing

# 4.2 
#new York 40.72845147394269, -74.07117285193736
# 4.3 
#Bronx 40.84540593445345, -73.86702825987568
#Brooklyn 40.677918029480615, -73.94312793158646
#Manhattan 40.785300587066295, -73.97060602404878
#Queens 40.731111856621645, -73.79978779242171
#Staten Island 40.57901018814548, -74.15191731362358
city <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
latitude <- c(40.84540593445345, 40.677918029480615, 40.785300587066295, 40.731111856621645, 40.57901018814548)
longitude <- c(-73.86702825987568, -73.94312793158646, -73.97060602404878, -73.79978779242171, -74.15191731362358)
distri <- c(1.2, 2.7, 15.1, 25, 2)
color <- c("black", "red", "green", "pink", "blue")

#4.4
#Para NY 
ggplot(data = world) +
  geom_sf()+
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(50, 10), expand = FALSE)

ggplot(data = world) +
  geom_sf() +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(17.65, 63.97), expand = FALSE)

data(world.cities)
names(world.cities)
head(world)
unique(world.cities)
View(world.cities)

usaCities <- data.frame(name = city, longitude = longitude
                     , latitude = latitude)
usaCities
ggplot(data = world) + 
  geom_sf(fill= "antiquewhite") + 
  coord_sf(xlim = c(-76.0, -70.5), ylim = c(40, 41.5), expand = FALSE) +
  geom_text(data= usaCities,aes(x=longitude, y=latitude, label=city),
            color = "black", fontface = "bold", check_overlap = FALSE) +
  annotation_scale(location = "bl", width_hint = 5) + 
  annotation_north_arrow(location = "bl", which_north = "true",pad_x = unit(0.3, "in"), pad_y = unit(0.9, "in")
                         ,style = north_arrow_fancy_orienteering) + 
  xlab("Longitude") + ylab("Latitude") + ggtitle("Cuidad de New York") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "white"))

##4.5
housing
totalB=data.frame( totalGSqFt =NA, Boro=NA)
for (clss in unique(housing$Boro))
{
  classdf <- housing %>%   filter(Boro == clss) %>%  summarise(sum(GSqFt))
  classdf$class <- clss
  names(classdf)<-c("totalGSqFt", "Boro")
  totalB <-rbind(totalB ,classdf)
}
totalB<- na.omit(totalB)
totalB$totalGSqFt <- round(totalB$totalGSqFt/100000000, 2) # en millones 
View(totalB)

usaCitiesB <- data.frame(name = city, longitude = longitude
                        , latitude = latitude, distri= distri, color=color)
usaCitiesB

ggplot(data = world) + 
  geom_sf(fill= "antiquewhite") + 
  coord_sf(xlim = c(-74.4, -73.5), ylim = c(40.4, 41.2), expand = FALSE) +
  geom_text(data= usaCities,aes(x=longitude, y=latitude, label=city),
            color = usaCitiesB$color, check_overlap = FALSE) +
  annotation_scale(location = "bl", width_hint = 5) + 
  annotation_north_arrow(location = "bl", which_north = "true",pad_x = unit(0.3, "in"), pad_y = unit(0.9, "in")
                         ,style = north_arrow_fancy_orienteering) + 
  xlab("Longitude") + ylab("Latitude") + ggtitle("Ciudad de New York") +
  geom_point(data = usaCities, aes(x = longitude, y = latitude), size = usaCitiesB$distri,shape = 10,
             fill = usaCitiesB$color) + theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "white"))

