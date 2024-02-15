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


##### Question 8

### 8a
marseille_L93$DISP_MED18_STD <- scale(marseille_L93$DISP_MED18)
summary(marseille_L93$DISP_MED18_STD)
var(marseille_L93$DISP_MED18_STD)


### 8b
moran.plot(as.numeric(marseille_L93$DISP_MED18_STD), listw=liste_pds)


### 8c
# En bas à gauche : autocorrélation positive et revenu médian faible => iris au revenu médian faible au milieu d'iris lui ressemblant
# En haut à droite : autocorrélation positive et revenu médian élevé => iris au revenu médian élevé au mileu d'iris lui ressembant
# En bas à droite : autocorrélation négative et revenu médian élevé => iris au revenu médian élevé au milieu d'iris au revenu médian plus faible
# En haut à gauche : autocorrélation négative et revenu médian faible => iris au revenu médian faible au milieu d'iris au revenu médian plus fort


### 8d
# On remarque bien une corrélation spatiale entre les revenus médian (régression linéaire qui semble plausible)
# Dans ce cas il s'agit d'une autocorrélation positive (pente positive + beacoup de points en bas à gauche et en haut à droite)


##### Question 9

### 9a
moran.test(as.numeric(marseille_L93$DISP_MED18_STD), liste_pds, randomisation = TRUE)


### 9b 
# On a Iw = 0.71 > 0 avec une p_valeur très faible donc le test nous dit bien qu'il y a une autocorrélation spatiale positive 
# Notre hypothèse est donc bien vérifiée



##### Question 10

### 10a
mars_rev_lisa <- localmoran(as.numeric(marseille_L93$DISP_MED18_STD), listw=liste_pds)


### 10b
class(mars_rev_lisa)
str(mars_rev_lisa, max.level = 1)
summary(mars_rev_lisa)


### 10c
# La moyenne des indicateurs locaux Ii est 0.71 = l'indicateur de Moran global


### 10d
mars_rev_lisa_nega <- data.frame(mars_rev_lisa) %>% 
  filter(Ii<0)
colnames(mars_rev_lisa_nega)[5] <- "p_value"
mars_rev_lisa_nega <- mars_rev_lisa_nega %>% 
  filter(p_value<0.05)
# => 2 iris en autocorrélation négative


### 10e
marseille_L93$LISA <- data.frame(mars_rev_lisa)$Ii
mf_map(x = marseille_L93, var = "LISA", type = "choro", breaks = "quantile")
mf_map(x = marseille_L93, var = "LISA", type = "choro", breaks = "fisher")
mf_map(x = marseille_L93, var = "LISA", type = "choro", breaks = "equal")


### 10f
# Les LISA semblent se regrouper : on retrouve l'autocorrélation positive 


### 10g
colnames(mars_rev_lisa)[5] <- "p_value"
marseille_L93$LISA_PVAL <- data.frame(mars_rev_lisa)$p_value


### 10h
signif <- subset(marseille_L93, LISA_PVAL < 0.05)
# 114 observations significatives (sur 336 iris)


### 10i
class_p_value <- c(0,0.01,0.05,0.1,1)
mf_map(x = marseille_L93, var = "LISA_PVAL", type = "choro", breaks = class_p_value)


### 10j
# On retrouve bien ce qu'on voulait





