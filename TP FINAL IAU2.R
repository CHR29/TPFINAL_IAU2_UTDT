#INSTRUMENTOS DE ANÁLISIS 2 
#UTDT - 2021
#ALUMNAS: ALVAREDO CONSTANZA, CORTIÑAS MAGDALENA, RABUFFETI CHIARA

#TRABAJO PRÁCTICO FINAL

#Contenido del trabajo
#Objetivo: Realizar un informe donde se pueda evidenciar el armado de un proyecto donde se pueda evidenciar las siguientes etapas:

#1. Pregunta y presentacion del tema

#2. Descripción de los pasos realizados

#3. Importación de datos a través de scrapping

#4. Limpieza y tratamiento de datos

#5. Visualizacion de la información. 

#6. Conclusión.

#1. 
#El objetivo de este trabajo es analizar las ventas de las propiedades en CABA. 
#Para ello nos enfocaremos en responder la siguiente pregunta: ¿Cuál es la relación entre al transporte público, más precisamente al subte y la venta de dptos en CABA? 
#En este sentido, procederemos a descargar la base de datos de Properati para observar los departamentos en venta y los datos del Cronista  para verificar los valores de los departamentos al día de hoy.

#3.4.
#Base de datos de Properati 2020_ departamentos en venta. 

departamentos.en.venta.2020 <- read.csv("Data/amba_properati_jun_jul_2020.csv", header=TRUE, sep=",",stringsAsFactors = TRUE)

departamentos.en.venta.2020

#A continuación limpiaremos el dataset, para quedarnos con las propiedades en venta en CABA y eliminar los posibles NA.  

departamentos.en.venta.2020 <- departamentos.en.venta.2020 %>% 
  filter(provincia == "CABA" & operation_type == "Venta")

departamentos.en.venta.2020 <- departamentos.en.venta.2020 %>% 
  filter(!is.na(lon))

#Cotización del dólar a la fecha del 26.9.2021

#Antes de incorporar los datos del Cronista para visualizar el valor del dólar, instalaremos algunas librerias que nos ayudarán a importarlo. 

library(tidyverse)
library(rvest) 
library(stringr) 

# Para poder analizar las propiedades en dolares, vamos a trabajar con la cotización del dia.
# Para esto tomaremos los datos de la pagina web del cronista y haremos un Scrapping

# Primero cargamos el URL de la pagina web

url <- "https://www.cronista.com/MercadosOnline/dolar.html" 

# Ahora vamos a ir importando la tabla por columnas
# Comenzamos por importar los datos que nos seran utiles para nuestra tabla. 
# Comenzamos con la informacion para ver el tipo de moneda y seguimos por el valor de las mismas

moneda <- read_html(url) %>%
  html_nodes(xpath = '//*[(@id = "market-scrll-1")]//*[contains(concat( " ", @class, " " ), concat( " ", "name", " " ))]') %>%
  html_text2()

valor <- read_html(url) %>% 
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "value", " " ))]') %>%
  html_text() %>%
  str_replace_all ("[^[:alnum:]]", "") %>%  
  as.numeric()

#Ahora armamos la tabla con estos datos

tabla <- tibble(moneda = moneda, valor= valor)

# Como vemos que al valor de la moneda le faltan los decimales se los agregamos

cotizacion <- tabla %>% 
  mutate(valor=valor/100) 

# Como para este trabajo nos interesa tomar de referencia el dolar blue, descartamos los otros valores

cotizacionDB <- filter(cotizacion, moneda=="DÓLAR BLUE")

# Comprobamos la estructura de los datos

str (cotizacionDB) 

# Una vez obtenida la cotizacion actualizada al dia de hoy del Dolar Blue atraves del scrappeo, vamos a guardarla como CSV ya que la misma puede ir varieando con el paso de los dias
# Antes vamos a agregarle una columna donde figure la fecha de cotizacion para no perder esa informacion que consideramos que a futuro puede ser relevante

USDBlue_ago21 <- mutate(cotizacionDB, fecha = "Agosto 21")

# A continuación la guardo: 

write.csv(USDBlue_ago21, "cotizacion.csv")

#Mapa - Barrios de CABA. 
#Previo a la carga del mapa, instalaremos otras librerías que nos permitan cargarlo y mapearlo. 

library(sf)
library(ggplot2)
library(ggmap)
options(scipen=99)

barrios <- read_sf("Data/barrios_caba.shp")

# Primero se crea una variable con la informacion sobre el CRS proyectado para la Ciudad Autonoma de Buenos Aires.
#Para poder configurar de la misma manera a todos los datos espaciales.

caba_proj = "+proj=tmerc +lat_0=-34.6297166 +lon_0=-58.4627 +k=1 +x_0=100000 +y_0=100000 +ellps=intl +units=m +no_defs"

# Se los transformara al sistema de coordenadas creado para la CABA

barrios <- st_transform(barrios, crs=caba_proj)

#Estaciones de subte. 

subte <- read_sf("Data/estaciones-de-subte.shp")

#A continuación realizaremos el mismo procedimiento hecho para los barrios, para los subtes.

subte <- st_transform(subte, crs=caba_proj)

#Siguiendo esta linea procederemos a incorporar los datos del Cronista en el dataset de Properati.

names (departamentos.en.venta.2020)[8] = "moneda"

departamentos.en.venta.2020  <- departamentos.en.venta.2020  %>% 
  mutate(moneda = str_replace_all(moneda, "USD", "USD BLUE"))

names (USDBlue_ago21)[1] = "moneda"

USDBlue_ago21  <- USDBlue_ago21  %>% 
  mutate(moneda = str_replace_all(moneda, "DÓLAR BLUE", "USD BLUE"))

departamentos.en.venta.2020.unido <- left_join(USDBlue_ago21, departamentos.en.venta.2020, by="moneda")




