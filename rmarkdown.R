---
  title: "Estructura general de RMD "
output: 
  html_document: 
  toc: yes
---
  
  # Uso de la Cabecera 
  
  La estructura básica de RMD es :
  Un encabezado YAML (opcional) rodeado por guiones --- 
  Uno o más fragmentos de código R conocidos como "chunks" rodeados por comillas simples ``` 
Uno o mas fragmentos de texto mezclado con formato de texto simple

El encabezado también conocido como YAML incluye algunas marcas como : 
  Author: 
  Title :
  Output:
  
  En la parte superior encontrará un ícono (ruedita) que configura donde sale la salida 

Todos los chunks deben ser ejecutados con el boton de run 

## Como ejecutar segmentos de código 

Agregue un nuevo fragmento haciendo clic en el botón * Insertar fragmento * en la barra de herramientas o presionando * Ctrl + Alt + I *. 


Vamos a agregar el código del laboratorio de coordenadas geodésicas


```{r maps}
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

#Primero cargamos el mapa de la tierra, 
world <- ne_countries(scale = "small", returnclass = "sf")
```


```{r}
robinson <- sf::st_transform(
  world,
  "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs"
)



ggplot(data = robinson) +
  geom_sf() +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Robinson")


```

## Como guardar la salida 


Cuando guarde el cuaderno, un archivo HTML que contiene el código y la salida se guardará junto a él (haga clic en el botón * Vista previa * o presione * Ctrl + Mayús + K * para obtener una vista previa del archivo HTML).

La vista previa muestra una copia HTML renderizada del contenido del editor. En consecuencia, a diferencia de * Knit *, * Preview * no ejecuta ningún fragmento de código R. En su lugar, se muestra la salida del fragmento cuando se ejecutó por última vez en el editor.

## Como mejorar la presentación 

El lenguaje de marcado contiene meta caracteres que son similares a aquellos de html 

### Listas Numeradas

Podemos crear listas numeradas únicamente indicando un  número  y punto : 
  ejemplo :Los siguientes  temas se deben tener en cuenta para crear un mapa

1.  Coordenadas Geodésicas 
2.  Proyección terrestre
1.  Sistemas de Coordenadas 

### Cambios de Estilos de Fuente 

Se puede configurar distintos estilos con el subrayado (_) 
_italica_ ,
__bold__  ,
___italica   bold___
Ejemplo 

Con la ayuda de los sistemas de referencia de coordenadas (CRS), cada lugar de la tierra se puede especificar mediante un conjunto de tres números, llamados coordenadas.
Que son : ___latitud___ ,  ___longitud___  y,  ___altura___  

### Creación de links

Se puede crear links tanto al mismo documento como a internet. Por ejemplo : 
  crs toma  el código PROJ4 que es el tipo de proyección espacial , para obtener referirse a : [spatial reference ](http://spatialreference.org/)


### Creación de fórmulas 

Para crear fórmulas debemos utilizar Latex. LaTeX es un sistema de composición tipográfica de alta calidad; incluye funcionalidades diseñadas para la producción de documentación técnica y científica. LaTeX es el estándar de facto para la comunicación y publicación de documentos científicos. LaTeX está disponible como software gratuito. 


$$
  \bar(x) = \sum_ { i=1}^n \frac{xi}{n}
$$
  Más información de la sintaxis Latex puede encontrar en : 
  [latex en 30 minutos ](https://www.overleaf.com/learn/latex/Learn_LaTeX_in_30_minutes)





# Creación de tablas  de datos

Muy fácil podemos hacerlo usando una función de knitr llamadas kable 

```{r}
library(knitr)
kable(mtcars[1:4,] , caption="Dataset de Mtcars")
```


# Como indicar el formato de salida 

El archivo puede generar salidas de distintos tipos. 

La cabecera del archivo tiene un argumento para indicar que tipo de archivo deberá generar. Sin embargo es tambien posible generar distintos  tipos de salida con el 
siguiente código
library(rmarkdown)
render("1-example.Rmd", output_format = "word_document")

Los siguientes formatos son configurables en el YAML :
  
  - html_notebook - Cuadernos R interactivos
- html_document: documento HTML con Bootstrap CSS
- pdf_document - documento PDF (a través de la plantilla LaTeX)
- word_document: documento de Microsoft Word (docx)
- odt_document - Documento de texto de OpenDocument
- rtf_document: documento con formato de texto enriquecido
- md_document - Documento de Markdown (varios sabores)



El botón de Knitr  del IDE procesa un archivo en el primer formato listado en su campo de salida. Puede renderizar en formatos adicionales haciendo clic en el menú desplegable al lado del botón :
  
  ## Presentaciones 
  
  Los siguientes formatos son configurables en el YAML 

- ioslides_presentation - presentación HTML con ioslides
- Revelajs :: Revelajs_presentation - Presentación HTML con Revel.js
- slidy_presentation - presentación HTML con W3C Slidy
- beamer_presentation - Presentación en PDF con LaTeX Beamer
- powerpoint_presentation: presentación de PowerPoint




# Como publicar el trabajo 

Es importante primero ejecutar cada una de las secciones de código R. Cada vez que se ejecuta se guarda un cache de la última renderización .

Ejecute el Preview y presione el botón de Publish al lado derecho de la ventana

Seleccione RPUB, necesitará un cuenta en RPUB, si no la tiene  podrá crearla inmediatamente   y guarde el URL resultante 

# CheatSheets

Para mayor información  revisar este [cheat sheet](https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf)

