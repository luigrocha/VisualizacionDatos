#LAB-COORDENADAS_GEODESICAS
#############################################################
#En este laboratorio veremos 
#1.- Coordenadas Geod�sicas 
#2.- Proyecci�n terrestre
#3.- Sistemas de Coordenadas 
#4.- Mapas con Facets
#############################################################
#Los mapas pueden agregar un contexto vital al incorporar muchas variables 
#en una visualizaci�n  f�cil de leer y aplicable. 
# El paquete ggplot2 base no maneja datos espaciales espec�ficamente.
# El estado actual de los objetos espaciales en R se basa en las clases espaciales 
#definidas en el paquete sp, pero el nuevo paquete sf ha implementado recientemente
#el est�ndar de "caracter�stica simples"
#Sp es preferible cuando se desea integrar con otros sistemas GIS
#Sf es m�s simple de usar, m�s intuitivo y m�s r�pido
rm(list=ls())
#install.packages(c("maps","rgeos","cowplot", "googleway", "ggrepel", "ggspatial",
#                   "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", "rnaturalearthhires"))
require(maps)
require(rgeos)
require(cowplot)
require(googleway)
require(ggrepel)
require(lwgeom)
#require(libwgeom)  esta no existe 
require(sf)
require(rnaturalearth)
require(rnaturalearthdata)
#require(rnaturalearthhires)  #este require Rtools pero Rtools no esta disponible para esta version R
require(devtools)
require("ggplot2")
#install_github("ropensci/rnaturalearthhires")  #posiblemente no necesitamos 
require("ggspatial")
install.packages("ggspatial")
install.packages("maps")
install.packages("rgeos")
install.packages("googleway")
install.packages("lwgeom")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("devtools")
install.packages("ggspatial")


#Primero cargamos el mapa de la tierra, 
world <- ne_countries(scale = "small", returnclass = "sf")
names(world)
sudamerica <- ne_countries(scale = "small", returnclass = "sf", continent="South America")

#Note los argumentos scale la escala puede ser small/medium/large
#Note el tipo de objeto que nos devuelve  puede ser sp(por defecto) o sf 
#Note las propiedades del objeto retornado contiene 64 campos sobre los pa�ses y territorios
#Veamos una primera visualizaci�n 
defplot <-   ggplot(data = world) +
  geom_sf()+
  ggtitle("Default")
defplot
#Note que esta proyecci�n presenta una gran distorci�n hacia los polos (Antartica)
#Pero podemos convertirle usando cualquier CRS o Proj4 
#Proj4js es una biblioteca de JavaScript para transformar coordenadas de puntos de un sistema de
#coordenadas a otro, incluidas las transformaciones de datums (puntos de referencia) incluidos .
#
#Para encontrar los c�digos proj4 debemos referirnos a : https://spatialreference.org/
#Abrir este URL , y buscar la transformacion deseada ej: Robinson,
#desplegar Proj4js format y copiar la transformaci�n a la funci�n

#Robinson y Winkel son proyecciones intermedias entre �reas Iguales y Conforme
robinson <- sf::st_transform(
  world,
  "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs"
)

#

robinsonplot<- ggplot(data = robinson) +
  geom_sf() +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Robinson")

robinsonplot

#Ahora con Mollweide que es de �reas iguales

mollweide <- sf::st_transform(
  world,
  "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
)

mollweideplot <-ggplot(data = mollweide) +
  geom_sf() +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Mollweide")

mollweideplot

#Ahora con mercator que es conforme
mercator <- sf::st_transform(
  world,
  "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
)

mercatorplot <-ggplot(data = mercator) +
  geom_sf() +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Mercator")

mercatorplot

#comparando 
plot_grid(defplot,robinsonplot,mollweideplot, mercatorplot)


#En el siguiente ejemplo obtenemos la cantidad de pa�ses de la data
ggplot(data = world) +
  geom_sf() +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Mapa de la Tierra ", subtitle = paste0("(",length(unique(world$name)), " paises)"))                

#El siguiente mapa en color verde y contraste con pa�ses en negro 
ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")
#El dataset tiene mucha informaci�n sobre el mundo, veamos un mapa con la poblaci�n

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "log10")

#El segundo tema que debemos tener en cuenta es el formato de las coordenadas, es decir como vamos
#especificar las localizaci�n de un punto que esta en la tierra que es esf�rica en un mapa 
#en el mapa de dos dimensiones, esto lo hacemos
#mediante sistemas de referencia de coordenadas geogr�ficas  o CRS

#Con la ayuda de los sistemas de referencia de coordenadas (CRS), cada lugar de la tierra se 
#puede especificar mediante un conjunto de tres n�meros, llamados coordenadas.Que son : latitud 
# longitud y, a veces, tambi�n un valor de altura para describir 
#una ubicaci�n en la superficie de la tierra. El m�s popular se llama WGS84. 

#Ademas de lo anterior el CRS contiene las unidades usadas para definir  los ejes X Y , los Datum que
#son los or�genes utilizado para colocar el sistema de coordenadas en el espacio y la informaci�n de
#la proyecci�n que es la ecuaci�n matem�tica utilizada para aplanar objetos que est�n en una superficie redonda
#para que pueda verlos en una superficie plana 



# La funci�n coord_sf() permite manejar el sistema de coordenadas, 

# De forma predeterminada, el mapa usar� el sistema de coordenadas de la 
#primera capa que defina, o por defecto usar� WGS84  (ver https://en.wikipedia.org/wiki/World_Geodetic_System)
#(latitud/longitud) y es el sistema de referencia usado en GPS


#Para cambiar la proyecci�n deberemos usar la funci�n coord_sf() y manipular
#el argumento crs. 
#crs puede tomar c�digos de varios tipos: 
# EPSG European Petroleum Survey Group EPSG 
# ETRS European Terrestrial Reference System 1989
# Proj4


#PROJ4 es un tipo antiguo pero todav�a puede ser usado . 
#Para obtenerlo referirse a https://spatialreference.org , buscar las proyecciones listadas
#seleccionar una proyecci�n y buscar el c�digos PROJ4 apropiado
#
# Se puede hacer referencia a un CRS mediante un n�mero SRID que forma parte de  c�digos EPSG. 

#Ejemplo
crs1 <- coord_sf(crs = "+proj=longlat +ellps=WGS84 +no_defs")
#Para Europa
#coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
#Para Ecuador 
#coord_sf(crs = "+proj=longlat +ellps=WGS84 +no_defs")
#Para Argentina
#+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs 
#


#Los  siguientes ejemplos son equivalentes. 

#La primera usando PROJ4 

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs ="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
#La segunda usando el SRID de EPSG
ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+init=epsg:3035")
#Y la tercera usando la funci�n st_crs para que  vaya y busque el c�digo EPSG
ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = st_crs(3035))


#La extensi�n del mapa tambi�n se puede establecer en coord_sf(), 
#lo que en la pr�ctica permite "hacer zoom" en el �rea de inter�s, 
#proporcionado por l�mites en el eje x (xlim) y en el eje y (ylim).

#Para Ecuador 
ggplot(data = world) +
  geom_sf()+
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(-7.65, 7.65), expand = FALSE)

#MAS PUNTOS DE INTER�S DEL ECUADOR EN EL URL https://www.geodatos.net/coordenadas/ecuador

europe <- worldmap[worldmap$continent == 'Europe',]
ggplot() + geom_sf(data = europe) + theme_bw()
#Para Centro America
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)


# Hay varios paquetes disponibles para crear la barra de escala en un mapa por ejemplo:
#prettymapr, vcd, ggsn o legendMap, para el presente lab usaremos  
#ggspatial, que proporciona funciones f�ciles de usar.

#La ubicaci�n de la barra de escala debe especificarse en longitud / latitud 
#en los argumentos lon y lat.
# La distancia sombreada dentro de la barra de escala est� controlada por el
#argumento distance_lon. mientras que su ancho est� determinado por distance_lat


ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97))

#location es localizaci�n de la escala (bl= bottom left , tl=top left )


#El mapa mundial ya contiene nombres de pa�ses y las coordenadas de un centroide
#de cada pa�s (entre otras informaci�nes). Las coordenadas del centroide son X y Y
# Podemos usar esta informaci�n para trazar nombres de pa�ses,
# La funci�n geom_text se puede usar para agregar una capa de texto a un mapa usando 
# coordenadas geogr�ficas.
sf::sf_use_s2(FALSE)   #no usar objetos s2
world_points<- st_centroid(world)
head(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

ggplot(data = world) +
  geom_sf() +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)


#MAPA COMPLETO  DEL CENTROAMERICA  
ggplot(data = world) + 
  geom_sf(fill= "antiquewhite") + 
  geom_text(data= world_points,aes(x=X, y=Y, label=name)
            , color = "darkblue", fontface = "bold" 
            , check_overlap = FALSE) + 
  annotate(geom = "text", x = -90, y = 26, label = "Centro America"
           , fontface = "italic", color = "grey22", size = 6) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true"
                         , pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in")
                         , style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97)
           ,expand = FALSE) + xlab("Longitude") + ylab("Latitude")+ 
  ggtitle("Mapa de CentroAmerica") + theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))


#MAPA COMPLETO  DEL ECUADOR
#A�adiendo  capa  ciudades 
#Creamos un data.frame que contiene las coordenadas
data(world.cities)
names(world.cities)

sitios <- data.frame(longitude = world.cities[world.cities$country.etc=="Ecuador",5]
                     , latitude = world.cities[world.cities$country.etc=="Ecuador",4],
                     , name = world.cities[world.cities$country.etc=="Ecuador",1])

sitios
ggplot(data = world) + 
  geom_sf(fill= "antiquewhite") + 
  geom_point(data = sitios, aes(x = longitude, y = latitude), size = 5, 
             shape = 10, fill = "darkred") +
  coord_sf(xlim = c(-85.0, -75.12), ylim = c(-4, 2), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true"
                         ,pad_x = unit(0.1, "in"), pad_y = unit(0.5, "in")
                         ,style = north_arrow_fancy_orienteering) + 
  xlab("Longitude") + ylab("Latitude") + ggtitle("Mapa de Ecuador") + theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))



####################################################################
#MAPA con FACETS  
####################################################################
#Hay veces que nos interesa ver  una evoluci�n de indicadores por cada
#elemento geogr�fico, en este caso podemos usar la funci�n facet 

world <- ne_countries(scale = "small", returnclass = "sf")
#Un poco de datawrangling 
world$gdp_md_est[is.na(world$gdp_md_est)] <- 0 
world$gdp_md_est[world$gdp_md_est<=20000] <- 1
world$gdp_md_est[world$gdp_md_est>20000  &  world$gdp_md_est<=100000] <- 2  
world$gdp_md_est[world$gdp_md_est>100000  &  world$gdp_md_est<=200000] <- 3
world$gdp_md_est[world$gdp_md_est>200000  &  world$gdp_md_est<=1000000] <- 4
world$gdp_md_est[world$gdp_md_est>1000000  &  world$gdp_md_est<=20000000] <- 5

world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))


centro <- subset(world , world$sov_a3 == "CAN" 
                 |world$sov_a3 == "US1" 
                 |world$sov_a3 == "MEX"  
                 |world$sov_a3 == "GTM"  
                 |world$sov_a3 == "SLV"  
                 |world$sov_a3 == "NIC"
                 |world$sov_a3 == "CRI"
                 |world$sov_a3 == "PAN"
                 |world$sov_a3 == "HND"
                 |world$sov_a3 == "BLZ" )

centro_points <- subset(world_points , world_points$sov_a3 == "CAN" 
                        |world_points$sov_a3 == "US1" 
                        |world_points$sov_a3 == "MEX"  
                        |world_points$sov_a3 == "GTM"  
                        |world_points$sov_a3 == "SLV"  
                        |world_points$sov_a3 == "NIC"
                        |world_points$sov_a3 == "CRI"
                        |world_points$sov_a3 == "PAN"
                        |world_points$sov_a3 == "HND"
                        |world_points$sov_a3 == "BLZ" )

#ESTE SALE M�S  FACETS PORQUE ESTA COLOCANDO NOMBRES DE WORLD POINTS
#QUE ESTAN EN TODO EL MUNDO , Y WORLDPOINTS TIENE GDP_MD_EST

ggplot(data = centro) + 
  geom_sf(fill= centro$gdp_md_est) + 
  geom_text(data= world_points,aes(x=X, y=Y, label=name) 
            , color = "darkblue", fontface = "bold" 
            , check_overlap = FALSE) +
  facet_wrap(vars(gdp_md_est))+
  annotation_scale(location = "bl", width_hint = 0.5) + 
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97)
           ,expand = FALSE) + xlab("Longitude") + ylab("Latitude")+ 
  ggtitle("Mapa de Centro America  Por GDP") + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))


#version con nombres y centroides




ggplot(data = centro) + 
  geom_sf(fill= centro$gdp_md_est) + 
  geom_text(data= centro_points,aes(x=X, y=Y, label=name)
            , color = "darkblue", fontface = "bold" 
            , check_overlap = FALSE)+
  facet_wrap(vars(gdp_md_est))+
  annotation_scale(location = "bl", width_hint = 0.5) + 
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 43.97)
           ,expand = FALSE) + xlab("Longitude") + ylab("Latitude")+ 
  ggtitle("Mapa de Centro America  Por GDP") + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))



#Algunos urls interesantes 

#https://mgimond.github.io/Spatial/coordinate-systems-in-r.html
#https://docs.qgis.org/3.22/en/docs/gentle_gis_introduction/coordinate_reference_systems.html
#https://geocompr.robinlovelace.net/reproj-geo-data.html