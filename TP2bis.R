rm(list=ls())

library(sf)
library(dplyr)
library(units)


### Question 1
commune <-st_read("TP2.R/fonds/commune_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252")
plot(st_geometry(commune), lwd = 0.1)

### Question 2
summary(commune)
str(commune)
# l'objet est un sf ET un dataframe


### Question 3
View(head(commune, n=10))
# Dernière colonne = coordonnées


### Question 4
st_crs(commune)
# Lambert93


### Question 5
commune_bretagne <- commune %>% 
  filter(reg == "53") %>% 
  select(c("code", "libelle","epc", "dep", "surf"))
# Colonne geometry non supprimée
# Bien filtrer sur la région car par dep pb


### Question 6
class(commune_bretagne)
# Toujours sf


### Question 7
plot(commune_bretagne, lwd = 0.1)


### Question 8
plot(st_geometry(commune_bretagne), lwd = 0.5)


### Question 9
commune_bretagne <- commune_bretagne %>% 
  mutate(surf2 = st_area(commune_bretagne))
# Unité en m²


### Question 10
commune_bretagne <- commune_bretagne %>% 
  mutate(surf2 = set_units(commune_bretagne$surf2, km^2))
# Unité transformée en km²


### Question 11
# Plusieurs raisons possibles : 
# Calcul sur des systèmes de projection différents (ce n'est pas le cas ici)
# Calcul sur des fonds différents : les fonds n'ont pas les mêmes précisions de contours.
# On trouve des fonds "simplifiés" des communes et des fonds détaillés (en général de la BDTOPO). Les fonds détaillés sont + précis et donc beaucoup plus lourds. 


### Question 12
dept_bretagne <- commune_bretagne %>% 
  group_by(dep) %>% 
  summarise(surf=sum(surf))
  
plot(st_geometry(dept_bretagne))


### Question 13
dept_bretagne2 <- commune_bretagne %>% 
  group_by(dep) %>% 
  summarise(geometry = st_union(geometry))

plot(st_geometry(dept_bretagne), axes=TRUE)
# c'est la méthode à préconiser pour regrouper des géométries- rien n'interdit de faire un summarise mais on n'a pas toujours une variable numérique à disposition.


### Question 14

#a
centroid_dept_bretagne <- st_centroid(dept_bretagne2)
class(centroid_dept_bretagne$geometry)
# Type point

#b
plot(st_geometry(dept_bretagne2))
plot(st_geometry(centroid_dept_bretagne), add =TRUE)
# Ne pas oublier les st_gemetry !!!!!

#c
centroid_dept_bretagne <- centroid_dept_bretagne %>% 
  mutate(dept_lib = c("Côtes-D'Armor", "Finistère", "Île-et-Vilaine", "Morbihan"))

#d
centroid_coords <- st_coordinates(centroid_dept_bretagne)
centroid_coords <- centroid_coords %>% 
  bind_cols(
    centroid_dept_bretagne %>% 
      select(dep, dept_lib) %>%
      st_drop_geometry()
  )
centroid_coords %>% str()

#e
plot(st_geometry(dept_bretagne))
plot(st_geometry(centroid_dept_bretagne), pch = 16, col = "red", add = TRUE)
text(
  x = centroid_coords$X,
  y = centroid_coords$Y,
  labels = centroid_coords$dept_lib,
  pos = 3,
  cex = 0.8,
  col = "red"
)


### Question 15
commune_centroid_bret <- st_intersects(commune_bretagne, centroid_dept_bretagne)
str(commune_centroid_bret)
commune_centre_dep<-commune_bretagne[which(lengths(commune_centroid_bret)>0),]


### Question 16
intersection <- st_intersection(commune_betagne, centroid_dept_bretagne)
str(intersection)
within <- st_within(commune_betagne, centroid_dept_bretagne)
str(within)




