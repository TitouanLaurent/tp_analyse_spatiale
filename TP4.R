rm(list=ls())
install.packages("mapsf")
set.seed(123)


# Chargement des packages
library(dplyr)
library(sf)
library(spdep)
library(RColorBrewer)
library(mapsf)



##### Question 1

# Import des données
iris<-st_read("iris_franceentiere_2021/iris_franceentiere_2021.shp")
data<-read.csv2("data/BASE_TD_FILO_DISP_IRIS_2018.csv",sep=";",dec=".")

# Jointure
marseille <- iris %>% 
  filter(substr(depcom,1,3)=="132") %>% 
  left_join(
    data %>% select(code=IRIS,DISP_MED18),
    by="code"
  )



##### Question 2
marseille_L93 <- st_transform(marseille, 2154)
plot(st_geometry(marseille_L93))
mf_map(marseille_L93)



##### Question 3
summary(marseille_L93)
hist(marseille_L93$DISP_MED18)

boxplot(marseille_L93$DISP_MED18 ~ marseille_L93$depcom)



##### Question 4
marseille_L93 <- marseille_L93 %>% 
  filter(!is.na(DISP_MED18))

mf_map(x = marseille_L93, var = "DISP_MED18", type = "choro", breaks = "quantile")
mf_map(x = marseille_L93, var = "DISP_MED18", type = "choro", breaks = "fisher")
mf_map(x = marseille_L93, var = "DISP_MED18", type = "choro", breaks = "equal")



##### Question 5

### 5a
marseille_L93$DISP_MED18_ALEA <- sample(marseille_L93$DISP_MED18)


### 5b
mf_map(x = marseille_L93, var = "DISP_MED18_ALEA", type = "choro", breaks = "quantile")
mf_map(x = marseille_L93, var = "DISP_MED18_ALEA", type = "choro", breaks = "fisher")
mf_map(x = marseille_L93, var = "DISP_MED18_ALEA", type = "choro", breaks = "equal")
# distribution aléatoire très différente de la distribution réelle



##### Question 6

### 6a
# Ici on semble avoir étudié une autocorrélation posivtive (les voisins se reseemblent)


### 6b
mat_conti <- spdep::poly2nb(marseille_L93$geometry, queen = TRUE)
summary(mat_conti)


### 6c
mat_conti[4]
# le 4e élément a 4 voisins


##### Question 7

### 7a
liste_pds <- spdep::nb2listw(mat_conti, zero.policy=TRUE)


### 7b
str(liste_pds, max.level = 1)
summary(liste_pds)


### 7c






