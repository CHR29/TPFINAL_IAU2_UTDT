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
#En este sentido, procederemos a descargar la base de datos de Properati para observar los departamentos en venta y los datos del Cronista para verificar los valores de los departamentos al día de hoy.

#Antes que nada cargaremos las librerias necesarias para realizar el trabajo
library(tidyverse)
library(rvest) 
library(stringr) 
library (sf)
library(ggplot2)
library(ggmap)
options(scipen=99) #De esta manera evitamos que aparezcan valores en notación cientifica a lo largo del trabajo


#3.4.
#Base de datos de Properati 2020_ departamentos en venta. 

departamentos.en.venta.2020 <- read.csv("amba_properati_jun_jul_2020(1).csv", header=TRUE, sep=",",stringsAsFactors = TRUE) #Cargamos el dataset de Properati

departamentos.en.venta.2020 #Lo visualizamos

#A continuación limpiaremos el dataset 

departamentos.en.venta.2020 <- departamentos.en.venta.2020 %>% 
  filter(provincia == "CABA" & operation_type == "Venta") #Filtramos los datos para quedarnos solamente con las propiedades localizadas en CABA que esten en venta 

departamentos.en.venta.2020 <- departamentos.en.venta.2020 %>% 
  filter(!is.na(lon)) #Filtramos aquellas observaciones que no tengan ubicacion geográfica

#Cotización del dólar a la fecha del 26.9.2021 

# Para poder analizar el valor actual de las propiedades en dolares, vamos a trabajar con la cotización del dia.
# Para esto tomaremos los datos de la pagina web del cronista y haremos un Scrapping

# Primero cargamos el URL de la pagina web

url <- "https://www.cronista.com/MercadosOnline/dolar.html" 

# Ahora vamos a ir importando la tabla por columnas
# Comenzamos por importar los datos que nos seran útiles para nuestra tabla. 
# Comenzamos con la información para ver el tipo de moneda y seguimos por el valor de las mismas

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

# Una vez obtenida la cotización actualizada al día de hoy del Dolar Blue atraves del scrappeo, vamos a guardarla como CSV ya que la misma puede ir varieando con el paso de los dias
# Antes vamos a agregarle una columna donde figure la fecha de cotizacion para no perder esa información que consideramos que a futuro puede ser relevante

USDBlue_ago21 <- mutate(cotizacionDB, fecha = "Agosto 21")

# A continuación la guardamos como un .csv: 

write.csv(USDBlue_ago21, "cotizacion.csv")

# Como queremos ver la informacion volcada en un mapa  de la ciudad y lo vamos a relacion a la red de subtes
# Descargamos los datasat con la información geografica de las comunas de CABA con la red de subtes y sus estaciones.

# Primero se crea una variable con la informacion sobre el CRS proyectado para la Ciudad Autónoma de Buenos Aires.
# Para poder configurar de la misma manera a todos los datos espaciales.

caba_proj = "+proj=tmerc +lat_0=-34.6297166 +lon_0=-58.4627 +k=1 +x_0=100000 +y_0=100000 +ellps=intl +units=m +no_defs"

# Se los transformara al sistema de coordenadas creado para la CABA
#Comunas

comuna.ok <- read_sf("comunas_wgs84.shp")

comuna.ok <- st_transform(comuna.ok, crs=caba_proj)

#Estaciones de subte. 

subte <- read_sf("estaciones-de-subte.shp")

#A continuación realizaremos el mismo procedimiento hecho para los barrios, para los subtes.

subte <- st_transform(subte, crs=caba_proj)

lineas <- read_sf("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/subte-estaciones/subte_lineas.geojson")

#Revisamos rapidamente los data set de las estaciones de subte y el de lineas para ver que coincidan

head(subte)
head(lineas)

# Las lineas de subte no estan escritas de la misma manera que en el data set con las estaciones, asi que lo modificamos 

estaciones <- subte %>% 
  mutate(LINEA = paste0("LINEA ", LINEA))

#Ahora, ya con todas las bases de datos cargadas procederemos a trabajarlas en conjunto para poder cruzar información y avanzar con en analisis

#Procedemos a incorporar los datos del Cronista en el dataset de Properati para estudiar el valor de los precios en su relacion dolar/pesos argentinos.

names (departamentos.en.venta.2020)[8] = "moneda" 

departamentos.en.venta.2020  <- departamentos.en.venta.2020  %>% 
  mutate(moneda = str_replace_all(moneda, "USD", "USD BLUE")) #reemplazamos la observación para dejar en claro que nos referimos al valor del Dolar Blue

names (USDBlue_ago21)[1] = "moneda"

USDBlue_ago21  <- USDBlue_ago21  %>% 
  mutate(moneda = str_replace_all(moneda, "DÓLAR BLUE", "USD BLUE")) #Hacemos la misma modificacion en el dataset de los datos descargados del cronista para que figure igual

departamentos.en.venta.2020.unido <- left_join(USDBlue_ago21, departamentos.en.venta.2020, by="moneda") #Unimos los data set

#A continuación agregaremos una nueva columna que nos indique el precio en pesos que se vendió el departamento al día de: 26.9.2021

departamentos.en.venta.2020.pesos <- departamentos.en.venta.2020.unido %>% 
  mutate (departamentos.en.venta.2020.unido, precioArg= price*valor)%>% #Creamos una nueva columna cuya informacion sale del calculo de multriplicar el precio en dolar por el valor del dolar blue
  mutate (departamentos.en.venta.2020.unido, precioXm2= price/surface_total) #Creamos una columna que nos indique el valor del precio cuadrado

#Una vez más limpiaremos nuestro Dataset con datos que consideramos irrelevantes para este analisis

departamentos.en.venta.2020.pesos <- select(departamentos.en.venta.2020.pesos, -fecha, -created_on)

#Al tener los datos unidos en tablas y limpios, empezaremos a graficar algunos datos. 

#Primero investigaremos el precio promedio por comuna 

departamentos.en.venta.2020.p.promedio <- departamentos.en.venta.2020.pesos %>% 
  mutate(price = as.numeric(price))  #Configuramos que que tome a la variable de la columna Price como numero y no como caracter

departamentos.en.venta.2020.p.promedio1 <- departamentos.en.venta.2020.p.promedio %>% 
  group_by(partido) %>%  #Agrupamos los datos por comuna
  summarise(precioprom=mean(price)) #Realizamos el promedio del valor de la propiedad

departamentos.en.venta.2020.p.promedio2 <- departamentos.en.venta.2020.p.promedio %>% 
  group_by(partido) %>% 
  summarise(preciopromm2=mean(precioXm2)) #En este caso realizamos el promedio del precio por metro cuadrado

#Procedemos a realizar un grafico que nos muestre de manera creciente el precio promedio por comuna 

ggplot(data=departamentos.en.venta.2020.p.promedio1)+
  geom_bar(aes(x=reorder(partido, -precioprom), #Configuramos para que efectivamente nos ordene las comunas segun como crece el precio promedio
               weight = precioprom, fill=precioprom))+
  coord_flip() + #Giramos los ejes para poder leer de manera mas clara las columnas 
  labs(title = "PRECIO PROMEDIO POR COMUNA", subtitle = "CABA", caption = "Fuente: PROPERATI", fill= "CANTIDAD",x="BARRIO",y="CANTIDAD") + #Agregamos las etiquetas al gráfico
  theme_classic()

#Podemos observar que la comuna 8 presenta el precio promedio más bajo de las propiedades vendidas, mientras que la 1 el más caro. 

#Ahora, con la misma logica, realizamos el gráfico para ver como es el precio promedio del metro cuadrado

ggplot(data=departamentos.en.venta.2020.p.promedio2)+
  geom_bar(aes(x=reorder(partido, -preciopromm2), weight= preciopromm2, fill=preciopromm2))+
  coord_flip() + 
  labs(title = "PRECIO PROMEDIO DEL M2 VENDIDAS POR COMUNA", subtitle = "CABA", caption = "Fuente: PROPERATI", fill= "CANTIDAD",x="BARRIO",y="CANTIDAD") +
  theme_classic()

#Podemos observar que la comuna 8 sigue presentando el precio promedio del m2 más bajo de las propiedades vendidas, pero en este caso la comuna 14 tiene los valores más caros. 

#Ahora analizaremos la cantidad de propiedades vendidos por comuna. 

departamentos.en.venta.2020.propXcomuna <- departamentos.en.venta.2020.pesos %>%
  group_by(partido) %>% #Volvemos a agrupar por comuna
  summarise(cantidad=n()) #Realizamos el calculo de la cantidad de observaciones por cada una, en este caso nos arrojara el total de propiedades 

ordenar <- arrange(departamentos.en.venta.2020.propXcomuna, desc(cantidad))

#Realizamos el un gráfico de barras que nos muestre los datos de manera ordenada segun la cantidad de propiedades por comuna
ggplot(departamentos.en.venta.2020.propXcomuna) + 
  geom_bar(aes(x=reorder(partido, -cantidad), weight=cantidad, fill = factor(cantidad))) +
  coord_flip() + 
  labs(title = "CANTIDAD DE PROPIEDADES VENDIDAS POR COMUNA", subtitle = "CABA", caption = "Fuente: PROPERATI", fill= "CANTIDAD",x="BARRIO",y="CANTIDAD") + 
  theme_classic()

#Es posible observar que la comuna 14 es en la  que más se vendieron propiedades.

#Ahora analizaremos el tipo de propiedad que más se vendió.
departamentos.en.venta.2020.tipo.prop <- departamentos.en.venta.2020.pesos %>%
  group_by(property_type) %>%
  summarise(cantidad=n())

ordenar <- arrange(departamentos.en.venta.2020.tipo.prop, desc(cantidad))

ggplot(departamentos.en.venta.2020.tipo.prop) +
  geom_bar(aes(x=property_type, weight=cantidad, fill = factor(cantidad))) + 
  coord_flip() +
  labs(title = "TIPO DE PROPIEDADES VENDIDAS", subtitle = "CABA", caption = "Fuente: PROPERATI", fill= "CANTIDAD",x="BARRIO",y="CANTIDAD") + 
  theme_classic()

#Es posible observar que el tipo de propiedad que tuvo una mayor demanda en el 2020 en CABA, fueron los departamentos.

#En este sentido analizaremos cuál es la comuna con mas casas, luego con más PHs y por último con más departamentos. 

departamentos.en.venta.2020.tipo.propXcomuna <- departamentos.en.venta.2020.pesos %>%
  group_by(partido, property_type) %>%
  summarise(cantidad=n())

#Para ver la la información diferenciada en distintos gráficos, por cada comuna, pero de manera conjunta realizaremos un faceteado

ggplot(departamentos.en.venta.2020.tipo.propXcomuna) + 
  geom_bar(aes(x=property_type, weight=cantidad, fill = property_type)) + 
  facet_wrap(~partido)+ 
  labs(title="TIPO DE PROPIEDADES VENDIDAS POR COMUNAS", subtitle = "CABA", x="TIPO DE PROPIEDAD", y="CANTIDAD", caption="Fuente: PROPERATI") 

#Es posible observar que la comuna 14 fue la que más vendió departamentos, la 12, 13 y 9 casas y la 14 y 15 PHS. 

#A continuación analizaremos en qué comuna se vendieron más monoambientes. 

departamentos.en.venta.2020.mono <- departamentos.en.venta.2020.pesos %>% 
  filter(rooms=="1") #Realizamos un filtro para quedarnosunicamente con las propiedades que tengan un ambiente

departamentos.en.venta.2020.mono1 <- departamentos.en.venta.2020.mono %>% 
  group_by(partido) %>%  #Los agrupamos por comuna
  summarise(cantidadmonoamb=n()) #Y contabilizamos en numero de observaciones

#Realizamos un gráfico

ggplot()+
  geom_bar(data=departamentos.en.venta.2020.mono1, 
           aes(x=reorder(partido, -cantidadmonoamb),
               weight = cantidadmonoamb, fill=cantidadmonoamb))+
  coord_flip() + 
  labs(title = "MONOAMBIENTES POR COMUNA", subtitle = "CABA", caption = "Fuente: PROPERATI", fill= "CANTIDAD",x="BARRIO",y="CANTIDAD") +
  theme_classic()

#Es posible observar que en la comuna 14 se vendieron más monoambientes, mientras que la 7 fue en la que vendió menos. 

#Por último haremos lo mismo pero para saber en qué comuna se vendieron más propiedades con más de 6 ambientes.
#Repetimos los pasos
departamentos.en.venta.2020.mas6 <- departamentos.en.venta.2020.pesos %>% 
  filter(rooms>=6)

departamentos.en.venta.2020.mas.6 <- departamentos.en.venta.2020.mas6 %>% 
  group_by(partido) %>% 
  summarise(cantidadamb=n())

#Realizamos el gráfico

ggplot()+
  geom_bar(data=departamentos.en.venta.2020.mas.6, 
           aes(x=reorder(partido, -cantidadamb),
               weight = cantidadamb, fill=cantidadamb))+
  coord_flip() + 
  labs(title = "PROPIEDADES CON + DE 6 AMB POR COMUNA", subtitle = "CABA", caption = "Fuente: PROPERATI", fill= "CANTIDAD",x="BARRIO",y="CANTIDAD") +
  theme_classic()

#A modo de conclusión podemos observar que también la comuna 14 fue en la que más se vendieron propiedades con más de 6 ambientes, no así en la comuna 8.

#A continuación nos detendremos a realizar un análisis georeferenciado de la información. 

#Primero mapearemos nuestro dataset de comunas para observar la geometría.

ggplot(comuna.ok)+
  geom_sf(fill="gray75", color="white") + #Creo que podemos agregar una capa para que nos apareza el nombre de las comunas
  labs(title = "COMUNAS CABA", caption = "Fuente: Gobierno de la Ciudad")+ 
  theme_void()

#Teniendo la geometría comenzaremos a mapear las propiedades vendidas en CABA. 

#Arpovechando que nuestro data set con los datos de propiedades tiene latitud y longitud para cada observación, lo transformamos en un dataset espacial
geoprop <- departamentos.en.venta.2020.pesos %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Mapeamos las comunas y la informacion de nuestro dataset de propiedades
ggplot()+
  geom_sf(data=comuna.ok, fill="gray75", color="white")+
  geom_sf(data=geoprop, aes(color=partido), alpha=0.5)+ 
  labs(title = "PROPIEDADES VENDIDAS", subtitle = "CABA 2020", caption = "Fuente: Gobierno de la Ciudad de Buenos Aires", x = "LATITUD", y = "LONGITUD")

# Continuaremos por estudiar el precio promedio de los comunas.
#Acomodaremos los dos dataset para poder unirlos y trabajarlos en conjunto
dptos.venta.y.precio.prom.m2 <- departamentos.en.venta.2020.pesos %>%
  left_join(departamentos.en.venta.2020.p.promedio2, by=c("partido"="partido"))

geo.prom.m2 <- dptos.venta.y.precio.prom.m2 %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Realizamos un par de pasos para que nos quede la columna con información de comunas escritas de igual modo y por unirlas
names (geo.prom.m2)[4] = "COMUNAS"

comuna <- mutate(comuna.ok, Comuna="Comuna")

comuna <- mutate(comuna, COMUNA=paste(Comuna, COMUNAS , sep=" "))

comuna<- select(comuna, -Comuna)

comuna <- st_transform(comuna, crs=caba_proj)
geo.prom.m2 <- st_transform(geo.prom.m2, crs=caba_proj)

# Ahora si, ya podemos realizar la unión
geo.precios <- comuna %>%
  st_join(geo.prom.m2, by=c("COMUNAS"="COMUNAS")) 

# Una vez unidas los dataset con la geometria,  procederemos a mapear la cantidad de departamentos vendidos por comuna y luego el precio promedio de los departamentos vendidos en el 2020 por comuna

geo.cantidad.comuna <- geo.precios %>% 
  group_by(COMUNA) %>% #Los agrupamos por comuna
  summarise(cantidad=n()) #Contabilizamos la cantidad de observaciones

#Realizamos el mapa para enteder como se distribuye

ggplot()+
  geom_sf(data=geo.cantidad.comuna, aes(fill=cantidad))+
  labs(title = "CANTIDAD DE PROPIEDADES VENDIDAS POR COMUNA", subtitle = "CABA 2020", caption = "Fuente:Properati", x = "LATITUD", y = "LONGITUD")

# De esta manera podemos observar que la comuna con mas propiedades vendidas en el 2020 fue la comuna 14

# Ahora realizamos los mismos pasos para ver el promedio del precio

geo.precio.comuna <- geo.precios %>% 
  group_by(COMUNA) %>% 
  summarise(promm2=mean(preciopromm2))

#Realizamos el mapa

ggplot()+
  geom_sf(data=geo.precio.comuna, aes(fill=promm2))+
  labs(title = "PROMEDIO DEL M2 POR COMUNA", subtitle = "CABA 2020", caption = "Fuente:Properati", x = "LATITUD", y = "LONGITUD")

# De esta manera podemos ver que las comunas del noreste de la ciudad (1, 2, 14 y 13) son las que vendieron a precios mas altos 
# Ahora queremos estudiar si hay relación con la cercania a las estaciones de subte y los valores de la propiedad

#Visualizamos un mapa de la ciudad y la red de subtes

ggplot()+
  geom_sf(data=filter(comuna.ok), colour="grey", alpha=0.2)+
  geom_sf(data = lineas, alpha = 0.6, linetype = "dashed", aes(color = LINEASUB)) +
  geom_sf(data = estaciones, alpha = 0.6, size=1.5,  aes(color = LINEA))+
  scale_color_manual(values = c("cyan3", "red","blue3",  "forestgreen", "darkorchid3", "yellow3" )) +
  labs(title = "Red de subtes",
       subtitle = "CABA",
       color = "Lineas de subte",
       caption= "Fuente: Buenos Aires Data") +
  theme_light()

# Dado que la Comuna 14 fue la que mas propiedades vendidas tuvo, con un mercado muy amplio (como vimos en el análisis fue la que más monoambientes y propiedades con más de 6 ambientes tuvo a la vente) y tambien una de las comunas con valores mas elevados, es que haremos foco en ella para estudiar el efecto del subte. 

# Primero lo que haremos es establecer la distancia que consideramos que se ve influenciada por las estaciones de subte
# En este caso tomaremos una distancia de 5 cuadras a la redonda, es decir 500 metros


coberturasubte <- st_buffer(subte, dist = 500) 

coberturasubte <- coberturasubte%>% summarise(cobertura=TRUE)

# Lo mapeamos

ggplot()+
  geom_sf(data=filter(comuna.ok), colour="grey")+
  geom_sf(data = lineas, alpha = 0.6, linetype = "dashed", aes(color = LINEASUB)) +
  geom_sf(data = estaciones, alpha = 0.6, size=1.5, aes(color = LINEA))+
  scale_color_manual(values = c("cyan3", "red","blue3",  "forestgreen", "darkorchid3", "yellow" )) +
  geom_sf(data= coberturasubte, colour="red", size=0.01,  alpha = 0.2)+
  labs(title = "Cobertura de red de subte",
       subtitle = "CABA",
       color = "Lineas de subte",
       caption= "Fuente: Buenos Aires Data")+
  theme_minimal() +
  coord_sf(datum=NA)

# Empezamos a volcar los datos de las propiedades de la Comuna 14


# Transformamos el data set en una espacial
geoprop <- departamentos.en.venta.2020.pesos %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Vemos como estan localizados las propiedades de comuna

ggplot()+
  geom_sf(data=comuna.ok, colour="grey")+
  geom_sf(data=filter(geoprop, partido=="Comuna 14"), aes(color=price), alpha=0.3)+
  labs(title = "Propiedades en Venta CABA",
       subtitle = "Comunas 14",
       color = "Precio USD",
       caption= "Fuente: Properatti") +
  scale_color_distiller(palette = "YlOrRd", direction = 1) +
  theme_light()

# Hacemos zoom en la Comuna para verlo mejor

ggplot()+
  geom_sf(data=filter(comuna.ok, COMUNAS=="14"), colour="grey")+
  geom_sf(data=filter(geoprop, partido=="Comuna 14"), aes(color=price), alpha=0.5)+
  labs(title = "Propiedades en Venta Comuna 14",
       subtitle = "Palermo",
       color = "Precio USD",
       caption= "Fuente: Portal de Datos Abiertos Argentina") +
  scale_color_distiller(palette = "YlOrRd", direction = 1) +
  theme_light()

# Ahora le superponemos la cobertura de subtes.
# Para esto deberemos filtrar primero y quedarnos con las estaciones de subte que pasan por la comuna y volver a calcular la cobertura

estacionesc14 <- estaciones %>% 
  filter(LINEA=="LINEA D") %>% 
  filter(ESTACION%in% c("AGÜERO", "BULNES", "R.SCALABRINI ORTIZ", "PLAZA ITALIA", "PALERMO", "MINISTRO CARRAZAN - MIGUEL ABUELO", "OLLEROS"))

estacionesc14 <-estacionesc14 %>% 
  mutate(barrio= "PALERMO")

# Mapeamos la red de subte que pasa por la comuna para poder visualizarla 

ggplot()+
  geom_sf(data=filter(comuna.ok, COMUNAS=="14"), colour="azure")+
  geom_sf(data = filter(lineas, LINEASUB=="LINEA D"), alpha = 0.3, linetype = "dashed") +
  geom_sf(data=estacionesc14, color = "darkolivegreen", size=3)+
  geom_sf_text(data=estacionesc14, aes(label= ESTACION), size=2.5, color="gray8" )+
  labs(title = "Red de subte Comuna 14",
       subtitle = "Palermo",
       caption= "Fuente: Buenos Aires Data") +
  theme_light()

# Ahora queremos ver como varia el precio de las propiedades al rededor de las estaciones de subte.
# Volvemos a tomar como radio de cobertura por estacion  500 m al rededor 

coberturasubtec14 <- st_buffer(estacionesc14, dist = 500) 

coberturasubtec14 <- coberturasubtec14%>% summarise(cobertura=TRUE)

# Vemos el readio de coberttura y como caria precio de las propiedades  

ggplot()+
  geom_sf(data=filter(comuna.ok, COMUNAS=="14"), colour="azure", alpha=0.7)+
  geom_sf(data=filter(geoprop, partido=="Comuna 14"), aes(color=price), size=1.5)+
  scale_color_distiller(palette = "Spectral",  breaks=c(500,2500,5500,9000, 12000)) +
  geom_sf(data= coberturasubtec14, colour="red", size=0.01,  alpha = 0.2,  linetype = "dashed")+
  geom_sf_text(data=estacionesc14, aes(label= ESTACION), size=2.5, color="gray8" )+
  labs(title = "Valor promedio de la m2 en la Comuna 14",
       subtitle = "Palermo",
       color = "Precio USD",
       caption= "Fuente: Properati") +
  theme_light()
#Vemos que en torno a las estaciones de subte el precio esta en valores intermedios

# Vemos como se da el precio promedio del metro cuadrado de las propiedades a ver si hay diferencias
ggplot()+
  geom_sf(data=filter(comuna.ok, COMUNAS=="14"), colour="azure", alpha=0.7)+
  geom_sf(data=filter(geoprop, partido=="Comuna 14"), aes(color=precioXm2), size=1.5)+
  scale_color_distiller(palette = "Spectral",  breaks=c(500,2500,5500,9000, 12000)) +
  geom_sf_text(data=estacionesc14, aes(label= ESTACION), size=2.5, color="gray8" )+
  geom_sf(data= coberturasubtec14, colour="red", size=0.01,  alpha = 0.2,  linetype = "dashed")+
  labs(title = "Valor promedio de la m2 en la Comuna 14",
       subtitle = "Palermo",
       color = "PrecioXm2 USD",
       caption= "Fuente: Properati") +
  theme_light()
# Vemos que sucede lo mismo, el valor de las propiedades aumenta hacia el noreste de la comuna, entendemos que el analisis se ve influenciado por la avenida libertador
# Una de las avenidas con precios mas caros de la ciudad por su.... 


#Ahora queremos ver como se distribuye el el tipo de propiedad en relacion a la cobertura de subte

ggplot()+
  geom_sf(data=filter(comuna.ok, COMUNAS=="14"), colour="azure", alpha=0.7)+
  geom_sf(data=filter(geoprop, partido=="Comuna 14"), aes(color=property_type), size=1.5)+
  scale_color_manual(values=c("darkolivegreen", "darkorange1", "darkorchid2"))+
  geom_sf_text(data=estacionesc14, aes(label= ESTACION), size=2.5, color="gray8" )+
  geom_sf(data= coberturasubtec14, colour="red", size=0.01,  alpha = 0.2,  linetype = "dashed")+
  labs(title = "Tipo de propiedad segun su cercania al subte",
       subtitle = "Comuna 14",
       color = "Tipo de Propiedad",
       caption= "Fuente: Properati") +
  theme_light()
#Vemos que si bien en la comuna predominan los departamentos, la mayoria de las casas no estan en el area de influencia del subte
# Esto se atribuye al caracter comercial que adquiere el area

# Ahora queremos ver como es el precio de los monoambientes, entendiendo que este es un mercado particular
ggplot()+
  geom_sf(data=filter(comuna.ok, COMUNAS=="14"), colour="azure", alpha=0.7)+
  geom_sf(data=filter(geoprop, partido=="Comuna 14" & rooms=="1"), aes(color=precioXm2), size=1.5)+
  geom_sf(data= coberturasubtec14, colour="red", size=0.01,  alpha = 0.2,  linetype = "dashed")+
  geom_sf_text(data=estacionesc14, aes(label= ESTACION), size=2.5, color="gray8" )+
  scale_color_distiller(palette = "YlOrRd", direction = 1,  breaks=c(500,2500,5500,9000, 12000)) +
  labs(title = "Tipo de propiedad segun su cercania al subte",
       subtitle = "Comuna 14",
       color = "Tipo de Propiedad",
       caption= "Fuente: Properati") +
  theme_light()

#Si bien la distribucion es bastante pareja encontramos algunos de valor elevado cerca a las estaciones de subte


