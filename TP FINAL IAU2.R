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

              
# Primero se crea una variable con la informacion sobre el CRS proyectado para la Ciudad Autonoma de Buenos Aires.
#Para poder configurar de la misma manera a todos los datos espaciales.

caba_proj = "+proj=tmerc +lat_0=-34.6297166 +lon_0=-58.4627 +k=1 +x_0=100000 +y_0=100000 +ellps=intl +units=m +no_defs"

# Se los transformara al sistema de coordenadas creado para la CABA

barrios <- st_transform(barrios, crs=caba_proj)

#Comunas

comunas.csv <- read.csv("Data/BARRIOXCOMUNA.csv", header=TRUE, sep=";",stringsAsFactors = TRUE) ????????????

comuna.ok <- read_sf("Data/comunas_wgs84.shp")

comuna.ok <- st_transform(comuna.ok, crs=caba_proj)

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

#A continuación agregaremos una nueva columna que nos indique el precio en pesos que se vendió el departamento al día de: 26.9.2021

departamentos.en.venta.2020.pesos <- departamentos.en.venta.2020.unido %>% 
  mutate (departamentos.en.venta.2020.unido, precioArg= price*valor)%>%
  mutate (departamentos.en.venta.2020.unido, precioXm2= price/surface_total)

#Una vez más limpiaremos nuesro Dataset 

departamentos.en.venta.2020.pesos <- select(departamentos.en.venta.2020.pesos, -fecha, -created_on)

#Al tener los datos unidos en tablas y limpios, empezaremos a graficar algunos datos. 

#Primero investigaremos el precio promedio por comuna 

departamentos.en.venta.2020.p.promedio <- departamentos.en.venta.2020.pesos %>% 
  mutate(price = as.numeric(price))  
  
departamentos.en.venta.2020.p.promedio1 <- departamentos.en.venta.2020.p.promedio %>% 
  group_by(partido) %>% 
  summarise(precioprom=mean(price))


departamentos.en.venta.2020.p.promedio2 <- departamentos.en.venta.2020.p.promedio %>% 
  group_by(partido) %>% 
  summarise(preciopromm2=mean(precioXm2))

ggplot()+
  geom_bar(data=departamentos.en.venta.2020.p.promedio1, 
           aes(x=reorder(partido, -precioprom),
               weight = precioprom, fill=precioprom))+
  coord_flip() + 
  labs(title = "PRECIO PROMEDIO POR COMUNA", subtitle = "CABA", caption = "Fuente: PROPERATI", fill= "CANTIDAD",x="BARRIO",y="CANTIDAD") +
  theme_classic()

#Podemos observar que la comuna 8 presenta el precio promedio más bajo de las propiedades vendidas, mientras que la 1 el más caro. 

ggplot()+
  geom_bar(data=departamentos.en.venta.2020.p.promedio2,
           aes(x=reorder(partido, -preciopromm2), weight= preciopromm2, fill=preciopromm2))+
  coord_flip() + 
  labs(title = "PRECIO PROMEDIO DEL M2 VENDIDAS POR COMUNA", subtitle = "CABA", caption = "Fuente: PROPERATI", fill= "CANTIDAD",x="BARRIO",y="CANTIDAD") +
  theme_classic()

#Podemos observar que la comuna 8 presenta el precio promedio del m2 más bajo de las propiedades vendidas, mientras que la 14 el más caro. 

#Ahora analizaremos la cantidad de propiedades vendidos por comuna. 

departamentos.en.venta.2020.propXcomuna <- departamentos.en.venta.2020.pesos %>%
  group_by(partido) %>%
  summarise(cantidad=n())

ordenar <- arrange(departamentos.en.venta.2020.propXcomuna, desc(cantidad))

ggplot(departamentos.en.venta.2020.propXcomuna) + geom_bar(aes(x=reorder(partido, -cantidad), weight=cantidad, fill = factor(cantidad))) + coord_flip() + labs(title = "CANTIDAD DE PROPIEDADES VENDIDAS POR COMUNA", subtitle = "CABA", caption = "Fuente: PROPERATI", fill= "CANTIDAD",x="BARRIO",y="CANTIDAD") + theme_classic()

#Es posible observar que la comuna 14 es en la  que más se vendieron propiedades.

#Ahora analizaremos el tipo de propiedad que más se vendió.

departamentos.en.venta.2020.tipo.prop <- departamentos.en.venta.2020.pesos %>%
  group_by(property_type) %>%
  summarise(cantidad=n())

ordenar <- arrange(departamentos.en.venta.2020.tipo.prop, desc(cantidad))

ggplot(departamentos.en.venta.2020.tipo.prop) + geom_bar(aes(x=property_type, weight=cantidad, fill = factor(cantidad))) + coord_flip() + labs(title = "TIPO DE PROPIEDADES VENDIDAS", subtitle = "CABA", caption = "Fuente: PROPERATI", fill= "CANTIDAD",x="BARRIO",y="CANTIDAD") + theme_classic()

#Es posible observar que el tipo de propiedad que tuvo una mayor demanda en el 2020 en CABA, fueron los departamentos.

#En este sentido analizaremos cuál es la comuna con mas casas, luego con más PHs y por último con más departamentos. 

departamentos.en.venta.2020.tipo.propXcomuna <- departamentos.en.venta.2020.pesos %>%
  group_by(partido, property_type) %>%
  summarise(cantidad=n())

ggplot(departamentos.en.venta.2020.tipo.propXcomuna) +  geom_bar(aes(x=property_type, weight=cantidad, fill = property_type)) + facet_wrap(~partido)+ labs(title="TIPO DE PROPIEDADES VENDIDAS POR COMUNAS", subtitle = "CABA", x="TIPO DE PROPIEDAD", y="CANTIDAD", caption="Fuente: PROPERATI") 

#Es posible observar que la comuna 14 fue la que más vendió departamentos, la 12, 13 y 9 casas y la 14 y 15 PHS. 

#A continuación analizaremos en qué comuna se vendieron más monoambientes. 

departamentos.en.venta.2020.mono <- departamentos.en.venta.2020.pesos %>% 
  filter(rooms=="1")

departamentos.en.venta.2020.mono1 <- departamentos.en.venta.2020.mono %>% 
  group_by(partido) %>% 
  summarise(cantidadmonoamb=n())

ggplot()+
  geom_bar(data=departamentos.en.venta.2020.mono1, 
           aes(x=reorder(partido, -cantidadmonoamb),
               weight = cantidadmonoamb, fill=cantidadmonoamb))+
  coord_flip() + 
  labs(title = "MONOAMBIENTES POR COMUNA", subtitle = "CABA", caption = "Fuente: PROPERATI", fill= "CANTIDAD",x="BARRIO",y="CANTIDAD") +
  theme_classic()

#Es posible observar que en la comuna 14 se vendieron más monoambientes, mientras que la 7 fue en la que vendió menos. 

#Por último estudiaremoss en qué comuna se vendieron más de 6 ambientes.

departamentos.en.venta.2020.mas6 <- departamentos.en.venta.2020.pesos %>% 
  filter(rooms>=6)

departamentos.en.venta.2020.mas.6 <- departamentos.en.venta.2020.mas6 %>% 
  group_by(partido) %>% 
  summarise(cantidadamb=n())

ggplot()+
  geom_bar(data=departamentos.en.venta.2020.mas.6, 
           aes(x=reorder(partido, -cantidadamb),
               weight = cantidadamb, fill=cantidadamb))+
  coord_flip() + 
  labs(title = "PROPIEDADES CON + DE 6 AMB POR COMUNA", subtitle = "CABA", caption = "Fuente: PROPERATI", fill= "CANTIDAD",x="BARRIO",y="CANTIDAD") +
  theme_classic()

#A modo de conclusión podemos observar que también la comuna 14 fue en la que más se vendieron propiedades con más de 6 ambientes, no así en la comuna 8.

#A continuación nos detendremos a realizar un aálisis georeferenciado de la información. 

#Primero mapearemos nuestro dataset de barrios para observar la geometría.

ggplot(barrios)+
  geom_sf(fill="gray75", color="white") + labs(title = "BARRIOS CABA", caption = "Fuente: Gobierno de la Ciudad")+ theme_void()

#Teniendo la geometría comenzaremos a mapear las propiedades vendidas en CABA. 

geoprop <- departamentos.en.venta.2020.pesos %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

ggplot()+
  geom_sf(data=barrios, fill="gray75", color="white")+
  geom_sf(data=geoprop, aes(color=partido), alpha=0.5)+ labs(title = "PROPIEDADES VENDIDAS", subtitle = "CABA 2020", caption = "Fuente: Gobierno de la Ciudad de Buenos Aires", x = "LATITUD", y = "LONGITUD")

#Continuaremos por estudiar el precio promedio de los comunas.

dptos.venta.y.precio.prom.m2 <- departamentos.en.venta.2020.pesos %>%
  left_join(departamentos.en.venta.2020.p.promedio2, by=c("partido"="partido"))

geo.prom.m2 <- dptos.venta.y.precio.prom.m2 %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

names (geo.prom.m2)[4] = "COMUNA"

comuna <- mutate(comunas.csv, Comuna="Comuna")

comuna <- mutate(comuna, COMUNA=paste(Comuna, COMUNA , sep=" "))

comuna<- select(comuna, -Comuna)

barrios.y.comunas <- comuna %>%
  left_join(barrios, by=c("BARRIO"="BARRIO"))

geo.byc <- barrios.y.comunas %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

geo.precios <- geo.prom.m2 %>%
st_join(geo.byc, by=c("COMUNA"="COMUNA"))





ggplot()+
  geom_sf(data=geo.prom.m2, aes(fill=preciopromm2), color="white") + labs(title = "VIVIENDA ASEQUIBLE", subtitle = "Barrios de la ciudad de Chicago", caption = "Fuente: Chicago Data Portal - https://data.cityofchicago.org/", x = "LATITUD", y = "LONGITUD") +  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  theme_light()

#En este sentido, ahora analizaremos en mayor detalle el precio promedio de los departamentos que se vendieron en 2020 separados por barrios. 




- mapa precio por comuna
- mapa precio por barrio
- mapa cercania a subtes 

- cantidad de dptos vendidos x comuna
- promedio de precio x comuna

- ver relacion como conclusion final: cantidad dptos y precio promedio








