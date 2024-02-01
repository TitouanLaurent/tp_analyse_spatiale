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
dept_bretagne
