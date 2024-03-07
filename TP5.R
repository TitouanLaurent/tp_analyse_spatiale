##################  TP5  ################
rm(list=ls())


##### Exercice 2 #####

### Question 1
library(DBI)
source(file = "connexion_db.R")
conn<-connecter()
DBI::dbListTables(conn)


### Question 2
DBI::dbListFields(conn,"popnaiss_com")


### Question 3
popnaiss <- dbSendQuery(conn, "SELECT * FROM popnaiss_com")
dbFetch(popnaiss) 
#type postgres


### Question 4
popnaiss2 <- dbGetQuery(conn, "SELECT * FROM popnaiss_com")
#type data.frame


### Question 5
dbFetch(dbSendQuery(conn, "SELECT * FROM popnaiss_com WHERE codgeo='35238';"))
dbGetQuery(conn, "SELECT * FROM popnaiss_com WHERE codgeo='35238';")


### Question 6
dbGetQuery(conn, "SELECT * FROM bpe21_metro INNER JOIN popnaiss_com ON bpe21_metro.depcom = popnaiss_com.codgeo WHERE codgeo='35047';")


### Question 7

#a
library(dplyr)
library(dbplyr)

# Connexion à la table popnaiss
popnaiss<-tbl(conn,"popnaiss_com")
str(popnaiss) # ! ce n'est pas un data.frame

# Reprise de la question 5
popnaiss %>% 
  filter(codgeo=="35047") %>% 
  show_query()

pop_bruz <- popnaiss %>% 
  filter(codgeo=="35047") %>% 
  collect()
str(pop_bruz)


#b
bpe_metro <- tbl(conn,"bpe21_metro")
result <- popnaiss %>%
  inner_join(bpe_metro, by = c("codgeo" = "depcom")) %>%
  filter(codgeo == "35047") %>% 
  show_query()
data.frame(result)


##### Exercice 3 #####

### Question 1
bpe_dep50 <- bpe_metro %>% 
  filter(dep == '35') %>% 
  select(c("id", "depcom", "dom", "typequ", "geometry")) %>% 
  show_query()
data.frame(bpe_dep50)


### Question 2
library(sf)
bpe_dep50 <- st_read(conn, query = "SELECT ID, DEPCOM, DOM, SDOM, TYPEQU, GEOMETRY FROM bpe21_metro WHERE DEP = '50'")


### Question 3
dbGetQuery(conn, "SELECT DISTINCT(ST_SRID(geometry)) FROM bpe21_metro;")
dbGetQuery(conn, "SELECT DISTINCT(ST_SRID(geometry)) FROM bpe21_04;")

st_crs(bpe_dep50)

data.frame(bpe_metro)
str(data.frame(bpe_metro))


### Question 4
st_read(conn, query = "SELECT COUNT(id) AS nb_mater, reg FROM bpe21_metro WHERE typequ='D107' GROUP BY reg ORDER BY nb_mater DESC")


### Question 5
#a
cinema_bpe <- st_read(conn, query = "SELECT * FROM bpe21_metro WHERE typequ='F303'")

#b
# On construit un buffer de 1km (une zone tampon) autour de la sorbonne
# df des coordonnées
sorbonne_buffer <- data.frame(x=2.34297,y=48.84864) %>% 
  #qu'on transforme en objet sf (systeme de proj WGS84 => crs=4326)
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  # on reprojette en LAMBERT-93 (crs=2154)
  st_transform(2154) %>% 
  # on crée la zone tampon autour du point (l'unité est le mètre ici)
  st_buffer(1000) 
str(sorbonne_buffer) # le buffer est constitué d'un unique polygône
plot(sorbonne_buffer %>% st_geometry()) # qui s'avère être un cercle

#c
cinema_1km_sorbonne_list <- st_within(cinema_bpe, sorbonne_buffer)
cinema_1km_sorbonne <- st_intersection(cinema_bpe, sorbonne_buffer)


### Question 6
library(leaflet)
# Optionnel :
# On récupère une icone spécifique sur https://ionic.io/ionicons (mot clé film)
cinemaIcons <- makeIcon(iconUrl = "film-outline.svg", 18,18)

leaflet() %>% 
  setView(lat = 48.84864, lng = 2.34297, zoom = 15) %>% 
  addTiles() %>% 
  addMarkers(lat = 48.84864, lng = 2.34297) %>% 
  addCircles(
    lat = 48.84864, lng = 2.34297, weight = 1, radius = 1000
  ) %>% 
  addMarkers(data = cinema_1km_sorbonne %>% st_transform(4326), icon = cinemaIcons)


# Remarque : 1000m en LAMBERT-93 ce n'est pas exactement 1000m en WGS84 (zoomez sur la carte suivante)
leaflet() %>%
  setView(lat = 48.84864, lng = 2.34297, zoom = 15) %>%
  addTiles() %>%
  addCircles(
    lat = 48.84864, lng = 2.34297, weight = 1, radius = 1000
  ) %>%
  addPolygons(data=sorbonne_buffer %>% st_transform(4326), col = "red")



##### Exercice 4 #####
















