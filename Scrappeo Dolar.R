
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(rvest) # Easily Harvest (Scrape) Web Pages
library(stringr) # Simple, Consistent Wrappers for Common String Operations


# Para poder analizar las propiedades en dolares, vamos a trabajar con la cotización del dia.
# Para esto tomaremos los datos de la pagina web del cronista y haremos un Scrapping


# Primero cargamos el URL de la pagina web
url <- "https://www.cronista.com/MercadosOnline/dolar.html" #cargo la URL


# Ahora vamos a ir importando la tabla por columnas
# Comenzamos por importar los datos que nos seran utiles para nuestra tabla. 
# Comenzamos con la informacion para ver el tipo de moneda y seguimos por el valor de las mismas
moneda <- read_html(url) %>%
  html_nodes(xpath = '//*[(@id = "market-scrll-1")]//*[contains(concat( " ", @class, " " ), concat( " ", "name", " " ))]') %>%
  html_text2()

valor <- read_html(url) %>% 
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "value", " " ))]') %>%
  html_text() %>%
  str_replace_all ("[^[:alnum:]]", "") %>% #elimino caracteres que no sean alfanuméticos 
  as.numeric()

#Ahora armamos la tabla con estos datos
tabla <- tibble(
  moneda = moneda,
   valor= valor)

# Como vemos que al valor de la moneda le faltan los decimales se los agregamos
cotizacion <- tabla %>% 
  mutate(valor=valor/100) #agregamos los decimales

# Como para este trabajo nos interesa tomar de referencia el dolar blue, descartamos los otros valores
cotizacionDB <- filter(cotizacion, moneda=="DÓLAR BLUE")

# Comprobamos la estructura de los datos
str (cotizacionDB) 

# Una vez obtenida la cotizacion actualizada al dia de hoy del Dolar Blue atraves del scrappeo, vamos a guardarla como CSV ya que la misma puede ir varieando con el paso de los dias
# Antes vamos a agregarle una columna donde figure la fecha de cotizacion para no perder esa informacion que consideramos que a futuro puede ser relevante

USDBlue_ago21 <- mutate(cotizacionDB, fecha = "Agosto 21")

# A continuación la guardo: 

write.csv(USDBlue_ago21, "cotizacion.csv")
