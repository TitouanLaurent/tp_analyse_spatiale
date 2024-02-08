##############  TP1  ###############

library(sf)
library(dplyr)
library(units)


# Question 1
commune <- st_read("fonds/commune_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252")


# Question 2
str(commune)
summary(commune)


# Question 3
View(head(commune, n=10))


# Question 4
st_crs(commune)


# Question 5
commune_Bretagne <- commune %>% 
  filter(dep=="29" | dep=="56"| dep=="22"| dep=="35") %>% 
  select(code, libelle, epc, dep, surf)


# Question 6
class(commune_Bretagne)


# Question 7
plot(commune_Bretagne)


# Question 8
plot(st_geometry(commune_Bretagne))


# Question 9
commune_Bretagne$surf2 <- st_area(commune_Bretagne$geometry)
str(commune_Bretagne$surf2)


# Question 10
commune_Bretagne$surf2 <- set_units(commune_Bretagne$surf2, km^2)


# Question 12
dept_bretagne <- commune_Bretagne %>%
  group_by(dep) %>% 
  summarise(surf2 = sum(surf2))
plot(dept_bretagne)


# Question 13
depart_bretagne <- commune_Bretagne %>%
  group_by(dep) %>% 
  summarise(geometry = st_union(geometry))
plot(depart_bretagne)


# Question 14
centroid_depart_bretagne <- st_centroid(depart_bretagne)
centroid_depart_bretagne <- st_sf(geometry = centroid_depart_bretagne)
print(centroid_depart_bretagne)

##a => type point

##b
plot(st_geometry(depart_bretagne))
plot(st_geometry(centroid_depart_bretagne), add = TRUE, col = "red")

##c
depart_names <- data.frame(dep = unique(depart_bretagne$dep),
                         dept_lib = c("Côtes d'Armor", "Finistère", "Île-et-Vilaine", "Morbihan"))
centroid_depart_bretagne <- merge(centroid_depart_bretagne, depart_names, by = "dep", all.x = TRUE)

##d
centroid_coords <- st_coordinates(centroid_depart_bretagne)
centroid_coords <- bind_cols(centroid_coords, centroid_depart_bretagne[, c("dep", "dept_lib")])
centroid_coords <- st_drop_geometry(centroid_coords)


##e
plot(st_geometry(depart_bretagne))
plot(st_geometry(centroid_depart_bretagne), add = TRUE, col = "red")
text(x = centroid_coords[, "X"], 
     y = centroid_coords[, "Y"], 
     labels = centroid_depart_bretagne$dept_lib, 
     pos = 1)


# Question 15
intersection <- st_intersects(centroid_depart_bretagne, commune)
commune %>%
  slice(unlist(intersection))


# Question 16
inter <- st_intersection(centroid_depart_bretagne, commune)
str(inter)
inter %>%
  select(libelle)


# Question 17
chefs_lieux <- data.frame(dep = c("22", "29", "35", "56"),
                          libelle = c("Saint-Brieuc", "Quimper", "Rennes", "Vannes"))
chefs_lieux <- merge(chefs_lieux, commune, by = "libelle", all.x = TRUE)

chef_lieux_coords <- st_coordinates(chefs_lieux)
distances <- st_distance(centroid_coords, chef_lieux_coords)


# Question 18









