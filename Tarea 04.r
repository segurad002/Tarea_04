#Cargamos los paquetes
library(sf)
library(raster)
library(dplyr)
library(rmapshaper)
library(spData)
library(leaflet)

#Carga de registros de Orquideas en Costa Rica 
orquideas_cr <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/gbif/orchidaceae-cr-registros.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )
st_crs(orquideas_cr) = 4326

# asp_cr <- rgdal::readOGR("https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/sinac/asp/asp-wgs84.geojson")
asp_cr <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/sinac/asp/asp-wgs84.geojson",
    quiet = TRUE
  )
st_crs(asp_cr) = 4326

#Se eliminan los registros con un valor mayor que 1000 de la capa de registros de pressencia
#Se eliminan los registros con valor vacÃ­o o NA en el campo species, en el conjunto de datos de registros de presencia
orquideas_cr <- filter(orquideas_cr, coordinateUncertaintyInMeters < 1000, !is.na(species), species != "")

#se eliminan los registros con valor de Area Marina de Manejo 
asp_cr <- filter(asp_cr, descripcio != "Area Marina de Manejo", descripcio != "Area marina protegida")


areas <- asp_cr %>% 
  st_make_valid() %>%
  st_join(orquideas_cr) %>% 
  group_by(nombre_asp) %>%
  summarize(species = n())


#VisualizaciÃ³n de  capas y controles
# Paleta de colores
colores_especies <-
  colorNumeric(palette = "YlOrRd",
               domain = areas$species,
               na.color = "transparent")
## Agregamos la capa base
leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = areas,
    fillColor = ~ colores_especies(areas$species),
    fillOpacity = 0.7,
    stroke = TRUE,
    color = "black",
    weight = 1,
    popup = paste(
      paste(
        "<strong>ASP:</strong>",
        areas$nombre_asp
      ),
      paste(
        "<strong>Cantidad de especies:</strong>",
        areas$species
        
      ),
      sep = '<br/>'
    ),
    group = "ASP - especies"
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = colores_especies,
    values = areas$species,
    group = "ASP - especies",
    title = "Cantidad de especies"
  ) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") %>%
  addProviderTiles(providers$OpenTopoMap, group = "Topografía") %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Stamen Toner Lite","Topografía"),
    overlayGroups = c("ASP - especies")
  )

## Agregamos una capa  para visualizar la cantidad de especies 
## Creamos una leyenda que muestre la relacion entre colores y cantidad de especies
## AÃ±adimos un control para activar y desactivar las capas 


.