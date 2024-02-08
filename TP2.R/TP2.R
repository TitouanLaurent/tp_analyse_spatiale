###############  TP2  ###############


# Chargement des packages
install.packages("openxlsx")

library(dplyr)
library(sf)
library(mapsf)
library(classInt)
library(leaflet)
library(openxlsx)
library(classInt)


#####  Exercice 1  #####


### Question 1

# Import des donnees 
# Fond communes France metropolitaine
communes_fm<- st_read("fonds/commune_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252") %>% 
  select(code,libelle,surf)
# Import des population légales des communes en 2019
pop_com_2019<-openxlsx::read.xlsx("fonds/Pop_legales_2019.xlsx")

# Correction pour la ville de Paris
pop_com_2019<-pop_com_2019 %>% 
  mutate(COM=if_else(substr(COM,1,3)=="751","75056",COM)) %>% 
  group_by(code=COM) %>% 
  summarise(pop=sum(PMUN19))

# Jointure
communes_fm<-communes_fm %>% 
  left_join(pop_com_2019,
            by="code") %>% 
  mutate(densite=pop/surf)


### Question 2
summary(communes_fm$densite)
hist(communes_fm$densite)


### Question 3
plot(communes_fm["densite"], border=FALSE)


### Question 4
plot(communes_fm["densite"], breaks="quantile", main="quantile", border = FALSE)
plot(communes_fm["densite"], breaks="sd", main="sd", border = FALSE)
plot(communes_fm["densite"], breaks="jenks", main="jenks", border = FALSE)
plot(communes_fm["densite"], breaks="pretty", main="pretty", border = FALSE)


### Question 5
denspop_quant <- classIntervals(
  communes_fm$densite,
  style = "quantile", 
  n = 5
)
str(denspop_quant)
head(denspop_quant$var)
denspop_quant$brks 

pal1 <- RColorBrewer::brewer.pal(n = 5, name = "YlOrRd")
plot(
  denspop_quant,
  pal = pal1,
  main = "quantile"
)

analyser_discret <- function(method, nb_classes){
  denspop_c <- classIntervals(
    communes_fm$densite,
    style = method, 
    n = nb_classes
  )
  print(denspop_c$brks)
  plot(
    denspop_c,
    pal = pal1,
    main = method
  )
  return(denspop_c)
}
# Avec cinq classes:
all_discret <- sapply(c("quantile", "sd","pretty","jenks"), analyser_discret, nb_classes = 5)


# A partir des informations obtenues, on peut définir nos propres intervalles. 
quantile(communes_fm$densite, probs = seq(0,1,0.1))
summary(communes_fm$densite)
#40 = médiane
#162 = moyenne
#on reprend certaines bornes de Jenks - en fusionnant les derniers intervalles
# Un exemple de découpage manuel avec 7 classes
denspop_man_brks7 <- c(0,40,162,500,1000,4000,8000,27200)
# Un exemple de découpage manuel avec 5 classes
denspop_man_brks5 <- c(0,40,162,1000,8000,27200)


popcomfm_sf <- communes_fm %>%
  mutate(
    densite_c = cut(
      densite,
      breaks = denspop_man_brks5,
      include.lowest = TRUE,
      right = FALSE,
      ordered_result = TRUE
    )
  )


table(popcomfm_sf$densite_c)
pal2 <- c(
  RColorBrewer::brewer.pal(
    n=5,
    name="Greens"
  )[4:3],
  RColorBrewer::brewer.pal(
    n=5,
    name="YlOrRd"
  )[c(2,4:5)]
)
plot(
  popcomfm_sf["densite_c"], 
  pal=pal2, 
  border = FALSE,
  main = "Densité de population",
)
